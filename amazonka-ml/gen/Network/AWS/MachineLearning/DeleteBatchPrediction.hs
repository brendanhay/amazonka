{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.DeleteBatchPrediction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Assigns the DELETED status to a 'BatchPrediction', rendering it unusable.
--
-- After using the 'DeleteBatchPrediction' operation, you can use the 'GetBatchPrediction' operation to verify that the status of the 'BatchPrediction' changed to
-- DELETED.
--
-- Caution The result of the 'DeleteBatchPrediction' operation is irreversible.
--
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteBatchPrediction.html>
module Network.AWS.MachineLearning.DeleteBatchPrediction
    (
    -- * Request
      DeleteBatchPrediction
    -- ** Request constructor
    , deleteBatchPrediction
    -- ** Request lenses
    , dbpBatchPredictionId

    -- * Response
    , DeleteBatchPredictionResponse
    -- ** Response constructor
    , deleteBatchPredictionResponse
    -- ** Response lenses
    , dbprBatchPredictionId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype DeleteBatchPrediction = DeleteBatchPrediction
    { _dbpBatchPredictionId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteBatchPrediction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbpBatchPredictionId' @::@ 'Text'
--
deleteBatchPrediction :: Text -- ^ 'dbpBatchPredictionId'
                      -> DeleteBatchPrediction
deleteBatchPrediction p1 = DeleteBatchPrediction
    { _dbpBatchPredictionId = p1
    }

-- | A user-supplied ID that uniquely identifies the 'BatchPrediction'.
dbpBatchPredictionId :: Lens' DeleteBatchPrediction Text
dbpBatchPredictionId =
    lens _dbpBatchPredictionId (\s a -> s { _dbpBatchPredictionId = a })

newtype DeleteBatchPredictionResponse = DeleteBatchPredictionResponse
    { _dbprBatchPredictionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DeleteBatchPredictionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbprBatchPredictionId' @::@ 'Maybe' 'Text'
--
deleteBatchPredictionResponse :: DeleteBatchPredictionResponse
deleteBatchPredictionResponse = DeleteBatchPredictionResponse
    { _dbprBatchPredictionId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'BatchPrediction'. This value
-- should be identical to the value of the 'BatchPredictionID' in the request.
dbprBatchPredictionId :: Lens' DeleteBatchPredictionResponse (Maybe Text)
dbprBatchPredictionId =
    lens _dbprBatchPredictionId (\s a -> s { _dbprBatchPredictionId = a })

instance ToPath DeleteBatchPrediction where
    toPath = const "/"

instance ToQuery DeleteBatchPrediction where
    toQuery = const mempty

instance ToHeaders DeleteBatchPrediction

instance ToJSON DeleteBatchPrediction where
    toJSON DeleteBatchPrediction{..} = object
        [ "BatchPredictionId" .= _dbpBatchPredictionId
        ]

instance AWSRequest DeleteBatchPrediction where
    type Sv DeleteBatchPrediction = MachineLearning
    type Rs DeleteBatchPrediction = DeleteBatchPredictionResponse

    request  = post "DeleteBatchPrediction"
    response = jsonResponse

instance FromJSON DeleteBatchPredictionResponse where
    parseJSON = withObject "DeleteBatchPredictionResponse" $ \o -> DeleteBatchPredictionResponse
        <$> o .:? "BatchPredictionId"
