{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Amazonka.DynamoDB.CompiledScan
    (
    -- * Creating a Request
      compileScan
    , CompiledScan

    -- * Request Lenses
    , csTableName
    , csIndexName
    , csProjectionExpression
    , csFilterExpression
    , csExpressionAttributeNames
    , csExpressionAttributeValues
    , csExclusiveStartKey
    , csTotalSegments
    , csSegment
    , csConsistentRead
    , csLimit
    , csReturnConsumedCapacity

    -- * Destructuring the Response
    , scanResponse
    , ScanResponse
    -- * Response Lenses
    , srsLastEvaluatedKey
    , srsCount
    , srsScannedCount
    , srsItems
    , srsConsumedCapacity
    , srsResponseStatus
    ) where

import Network.AWS.DynamoDB

import Amazonka.DynamoDB.Expression.Compile
import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Item.Value          (Value, getValue)

import Control.Lens.Zoom                (zoom)
import Control.Monad.Trans.State.Strict (runState)

import Data.Bifunctor         (bimap)
import Data.HashMap.Strict    (HashMap)
import Data.Monoid
import Data.Proxy
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Tuple             (swap)

import Network.AWS.DynamoDB
import Network.AWS.Lens
import Network.AWS.Pager    (AWSPager (..))
import Network.AWS.Prelude  (ToHeaders, ToJSON, ToPath, ToQuery, ToText (..))
import Network.AWS.Request  (coerceRequest)
import Network.AWS.Types    (AWSRequest (..))

import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict    as Map
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as Build

newtype CompiledScan = UnsafeScan Scan
    deriving (ToHeaders, ToPath, ToQuery, ToJSON)

_UnsafeScan :: Iso' CompiledScan Scan
_UnsafeScan = iso (\(UnsafeScan a) -> a) UnsafeScan

compileScan :: Text                            -- ^ Table name.
            -> ConditionExpression  Name Value -- ^ A filter expression.
            -> ProjectionExpression Name       -- ^ A projection expression.
            -> CompiledScan
compileScan table cond proj =
    let ((proj', cond'), (names, values)) =
            flip runState (mempty, mempty) $
                (,) <$> bicompile conditionExpression cond
                    <*> zoom _1 (compile projectionExpression proj)
     in UnsafeScan $ scan table
            & sProjectionExpression      ?~ finalize       proj'
            & sFilterExpression          ?~ finalize       cond'
            & sExpressionAttributeNames  .~ finalizeNames  names
            & sExpressionAttributeValues .~ finalizeValues values

instance AWSRequest CompiledScan where
    type Rs CompiledScan = Rs Scan

    request        = coerceRequest . request
    response l s _ = response l s (Proxy :: Proxy Scan)

instance AWSPager CompiledScan where
    page rq = fmap UnsafeScan . page (rq ^. _UnsafeScan)

-- | The name of the table containing the requested items; or, if you provide 'IndexName', the name of the table to which that index belongs.
csTableName :: Lens' CompiledScan Text
csTableName = _UnsafeScan . sTableName

-- | The name of a secondary index to scan. This index can be any local secondary index or global secondary index. Note that if you use the 'IndexName' parameter, you must also provide 'TableName'.
csIndexName :: Lens' CompiledScan (Maybe Text)
csIndexName = _UnsafeScan . sIndexName

csProjectionExpression :: Getter CompiledScan (Maybe Text)
csProjectionExpression = _UnsafeScan . sProjectionExpression

csFilterExpression :: Getter CompiledScan (Maybe Text)
csFilterExpression = _UnsafeScan . sFilterExpression

csExpressionAttributeNames :: Getter CompiledScan (HashMap Text Text)
csExpressionAttributeNames = _UnsafeScan . sExpressionAttributeNames

csExpressionAttributeValues :: Getter CompiledScan (HashMap Text AttributeValue)
csExpressionAttributeValues = _UnsafeScan . sExpressionAttributeValues

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for /LastEvaluatedKey/ in the previous operation.
--
-- The data type for /ExclusiveStartKey/ must be String, Number or Binary. No
-- set data types are allowed.
--
-- In a parallel scan, a /Scan/ request that includes /ExclusiveStartKey/ must
-- specify the same segment whose previous /Scan/ returned the corresponding
-- value of /LastEvaluatedKey/.
csExclusiveStartKey :: Lens' CompiledScan (HashMap Text AttributeValue)
csExclusiveStartKey = _UnsafeScan . sExclusiveStartKey

-- | For a parallel /Scan/ request, /TotalSegments/ represents the total number
-- of segments into which the /Scan/ operation will be divided. The value of
-- /TotalSegments/ corresponds to the number of application workers that will
-- perform the parallel scan. For example, if you want to use four application
-- threads to scan a table or an index, specify a /TotalSegments/ value of 4.
--
-- The value for /TotalSegments/ must be greater than or equal to 1, and less
-- than or equal to 1000000. If you specify a /TotalSegments/ value of 1, the
-- /Scan/ operation will be sequential rather than parallel.
--
-- If you specify /TotalSegments/, you must also specify /Segment/.
csTotalSegments :: Lens' CompiledScan (Maybe Natural)
csTotalSegments = _UnsafeScan . sTotalSegments

-- | For a parallel /Scan/ request, /Segment/ identifies an individual segment
-- to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For example,
-- if you want to use four application threads to scan a table or an index,
-- then the first thread specifies a /Segment/ value of 0, the second thread
-- specifies 1, and so on.
--
-- The value of /LastEvaluatedKey/ returned from a parallel /Scan/ request must
-- be used as /ExclusiveStartKey/ with the same segment ID in a subsequent
-- /Scan/ operation.
--
-- The value for /Segment/ must be greater than or equal to 0, and less than
-- the value provided for /TotalSegments/.
--
-- If you provide /Segment/, you must also provide /TotalSegments/.
csSegment :: Lens' CompiledScan (Maybe Natural)
csSegment = _UnsafeScan . sSegment

-- | A Boolean value that determines the read consistency model during the scan:
--
-- -   If /ConsistentRead/ is 'false', then the data returned from /Scan/ might not contain the results from other recently completed write operations (PutItem, UpdateItem or DeleteItem).
--
-- -   If /ConsistentRead/ is 'true', then all of the write operations that completed before the /Scan/ began are guaranteed to be contained in the /Scan/ response.
--
-- The default setting for /ConsistentRead/ is 'false'.
--
-- The /ConsistentRead/ parameter is not supported on global secondary indexes. If you scan a global secondary index with /ConsistentRead/ set to true, you will receive a /ValidationException/.
csConsistentRead :: Lens' CompiledScan (Maybe Bool)
csConsistentRead = _UnsafeScan . sConsistentRead

-- | The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in /LastEvaluatedKey/ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed data set size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in /LastEvaluatedKey/ to apply in a subsequent operation to continue the operation. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan> in the /Amazon DynamoDB Developer Guide/.
csLimit :: Lens' CompiledScan (Maybe Natural)
csLimit = _UnsafeScan . sLimit

-- | Undocumented member.
csReturnConsumedCapacity :: Lens' CompiledScan (Maybe ReturnConsumedCapacity)
csReturnConsumedCapacity = _UnsafeScan . sReturnConsumedCapacity
