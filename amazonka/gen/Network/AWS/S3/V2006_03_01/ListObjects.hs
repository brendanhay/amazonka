{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
module Network.AWS.S3.V2006_03_01.ListObjects where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

type GetBucket = ListObjects

-- | Minimum specification for a 'ListObjects' request.
listObjects :: BucketName -- ^ '_lorBucket'
            -> ListObjects
listObjects p1 = ListObjects
    { _lorBucket = p1
    , _lorDelimiter = Nothing
    , _lorEncodingType = Nothing
    , _lorMarker = Nothing
    , _lorMaxKeys = Nothing
    , _lorPrefix = Nothing
    }

data ListObjects = ListObjects
    { _lorBucket :: BucketName
    , _lorDelimiter :: Maybe Char
      -- ^ A delimiter is a character you use to group keys.
    , _lorEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and
      -- specifies the encoding method to use. An object key may contain
      -- any Unicode character; however, XML 1.0 parser cannot parse some
      -- characters, such as characters with an ASCII value from 0 to 10.
      -- For characters that are not supported in XML 1.0, you can add
      -- this parameter to request that Amazon S3 encode the keys in the
      -- response.
    , _lorMarker :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
    , _lorMaxKeys :: Maybe Integer
      -- ^ Sets the maximum number of keys returned in the response. The
      -- response might contain fewer keys but will never contain more.
    , _lorPrefix :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
    } deriving (Generic)

instance ToPath ListObjects where
    toPath ListObjects{..} = mconcat
        [ "/"
        , toBS _lorBucket
        ]

instance ToQuery ListObjects

instance ToHeaders ListObjects

instance ToBody ListObjects

instance AWSRequest ListObjects where
    type Sv ListObjects = S3
    type Rs ListObjects = ListObjectsResponse

    request = get

    response _ = xmlResponse

instance AWSPager ListObjects where
    next rq rs
        | not (_looIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lorMarker = fmap (toText . _oKey) . listToMaybe $ _looContents rs
            }

data ListObjectsResponse = ListObjectsResponse
    { _looIsTruncated :: Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of
      -- the results that satisfied the search criteria.
    , _looName :: Maybe BucketName
    , _looCommonPrefixes :: [CommonPrefix]
    , _looEncodingType :: Maybe EncodingType
      -- ^ Encoding type used by Amazon S3 to encode object keys in the
      -- response.
    , _looMarker :: Maybe Text
    , _looMaxKeys :: Maybe Integer
    , _looNextMarker :: Maybe Text
      -- ^ When response is truncated (the IsTruncated element value in the
      -- response is true), you can use the key name in this field as
      -- marker in the subsequent request to get next set of objects.
      -- Amazon S3 lists objects in alphabetical order Note: This element
      -- is returned only if you have delimiter request parameter
      -- specified. If response does not include the NextMaker and it is
      -- truncated, you can use the value of the last Key in the response
      -- as the marker in the subsequent request to get the next set of
      -- object keys.
    , _looContents :: [Object]
    , _looPrefix :: Maybe Text
    } deriving (Generic)

instance FromXML ListObjectsResponse where
    fromXMLOptions = xmlOptions
