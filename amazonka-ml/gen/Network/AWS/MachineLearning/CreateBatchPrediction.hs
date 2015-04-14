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

-- Module      : Network.AWS.MachineLearning.CreateBatchPrediction
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

-- | Generates predictions for a group of observations. The observations to
-- process exist in one or more data files referenced by a 'DataSource'. This
-- operation creates a new 'BatchPrediction', and uses an 'MLModel' and the data
-- files referenced by the 'DataSource' as information sources.
--
-- 'CreateBatchPrediction' is an asynchronous operation. In response to 'CreateBatchPrediction', Amazon Machine Learning (Amazon ML) immediately returns and sets the 'BatchPrediction' status to 'PENDING'. After the 'BatchPrediction' completes, Amazon ML sets the
-- status to 'COMPLETED'.
--
-- You can poll for status updates by using the 'GetBatchPrediction' operation
-- and checking the 'Status' parameter of the result. After the 'COMPLETED' status
-- appears, the results are available in the location specified by the 'OutputUri'
-- parameter.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_CreateBatchPrediction.html>
module Network.AWS.MachineLearning.CreateBatchPrediction
    (
    -- * Request
      CreateBatchPrediction
    -- ** Request constructor
    , createBatchPrediction
    -- ** Request lenses
    , cbpBatchPredictionDataSourceId
    , cbpBatchPredictionId
    , cbpBatchPredictionName
    , cbpMLModelId
    , cbpOutputUri

    -- * Response
    , CreateBatchPredictionResponse
    -- ** Response constructor
    , createBatchPredictionResponse
    -- ** Response lenses
    , cbprBatchPredictionId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data CreateBatchPrediction = CreateBatchPrediction
    { _cbpBatchPredictionDataSourceId :: Text
    , _cbpBatchPredictionId           :: Text
    , _cbpBatchPredictionName         :: Maybe Text
    , _cbpMLModelId                   :: Text
    , _cbpOutputUri                   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateBatchPrediction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbpBatchPredictionDataSourceId' @::@ 'Text'
--
-- * 'cbpBatchPredictionId' @::@ 'Text'
--
-- * 'cbpBatchPredictionName' @::@ 'Maybe' 'Text'
--
-- * 'cbpMLModelId' @::@ 'Text'
--
-- * 'cbpOutputUri' @::@ 'Text'
--
createBatchPrediction :: Text -- ^ 'cbpBatchPredictionId'
                      -> Text -- ^ 'cbpMLModelId'
                      -> Text -- ^ 'cbpBatchPredictionDataSourceId'
                      -> Text -- ^ 'cbpOutputUri'
                      -> CreateBatchPrediction
createBatchPrediction p1 p2 p3 p4 = CreateBatchPrediction
    { _cbpBatchPredictionId           = p1
    , _cbpMLModelId                   = p2
    , _cbpBatchPredictionDataSourceId = p3
    , _cbpOutputUri                   = p4
    , _cbpBatchPredictionName         = Nothing
    }

-- | The ID of the 'DataSource' that points to the group of observations to predict.
cbpBatchPredictionDataSourceId :: Lens' CreateBatchPrediction Text
cbpBatchPredictionDataSourceId =
    lens _cbpBatchPredictionDataSourceId
        (\s a -> s { _cbpBatchPredictionDataSourceId = a })

-- | A user-supplied ID that uniquely identifies the 'BatchPrediction'.
cbpBatchPredictionId :: Lens' CreateBatchPrediction Text
cbpBatchPredictionId =
    lens _cbpBatchPredictionId (\s a -> s { _cbpBatchPredictionId = a })

-- | A user-supplied name or description of the 'BatchPrediction'. 'BatchPredictionName' can only use the UTF-8 character set.
cbpBatchPredictionName :: Lens' CreateBatchPrediction (Maybe Text)
cbpBatchPredictionName =
    lens _cbpBatchPredictionName (\s a -> s { _cbpBatchPredictionName = a })

-- | The ID of the 'MLModel' that will generate predictions for the group of
-- observations.
cbpMLModelId :: Lens' CreateBatchPrediction Text
cbpMLModelId = lens _cbpMLModelId (\s a -> s { _cbpMLModelId = a })

-- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or
-- directory to store the batch prediction results. The following substrings are
-- not allowed in the s3 key portion of the "outputURI" field: ':', '//', '/./',
-- '/../'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your behalf.
-- For information about how to set permissions, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine LearningDeveloper Guide>.
cbpOutputUri :: Lens' CreateBatchPrediction Text
cbpOutputUri = lens _cbpOutputUri (\s a -> s { _cbpOutputUri = a })

newtype CreateBatchPredictionResponse = CreateBatchPredictionResponse
    { _cbprBatchPredictionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateBatchPredictionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbprBatchPredictionId' @::@ 'Maybe' 'Text'
--
createBatchPredictionResponse :: CreateBatchPredictionResponse
createBatchPredictionResponse = CreateBatchPredictionResponse
    { _cbprBatchPredictionId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'BatchPrediction'. This value
-- is identical to the value of the 'BatchPredictionId' in the request.
cbprBatchPredictionId :: Lens' CreateBatchPredictionResponse (Maybe Text)
cbprBatchPredictionId =
    lens _cbprBatchPredictionId (\s a -> s { _cbprBatchPredictionId = a })

instance ToPath CreateBatchPrediction where
    toPath = const "/"

instance ToQuery CreateBatchPrediction where
    toQuery = const mempty

instance ToHeaders CreateBatchPrediction

instance ToJSON CreateBatchPrediction where
    toJSON CreateBatchPrediction{..} = object
        [ "BatchPredictionId"           .= _cbpBatchPredictionId
        , "BatchPredictionName"         .= _cbpBatchPredictionName
        , "MLModelId"                   .= _cbpMLModelId
        , "BatchPredictionDataSourceId" .= _cbpBatchPredictionDataSourceId
        , "OutputUri"                   .= _cbpOutputUri
        ]

instance AWSRequest CreateBatchPrediction where
    type Sv CreateBatchPrediction = MachineLearning
    type Rs CreateBatchPrediction = CreateBatchPredictionResponse

    request  = post "CreateBatchPrediction"
    response = jsonResponse

instance FromJSON CreateBatchPredictionResponse where
    parseJSON = withObject "CreateBatchPredictionResponse" $ \o -> CreateBatchPredictionResponse
        <$> o .:? "BatchPredictionId"
