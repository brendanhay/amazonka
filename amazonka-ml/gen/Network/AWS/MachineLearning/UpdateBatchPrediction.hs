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

-- Module      : Network.AWS.MachineLearning.UpdateBatchPrediction
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

-- | Updates the 'BatchPredictionName' of a 'BatchPrediction'.
--
-- You can use the 'GetBatchPrediction' operation to view the contents of the
-- updated data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateBatchPrediction.html>
module Network.AWS.MachineLearning.UpdateBatchPrediction
    (
    -- * Request
      UpdateBatchPrediction
    -- ** Request constructor
    , updateBatchPrediction
    -- ** Request lenses
    , ubpBatchPredictionId
    , ubpBatchPredictionName

    -- * Response
    , UpdateBatchPredictionResponse
    -- ** Response constructor
    , updateBatchPredictionResponse
    -- ** Response lenses
    , ubprBatchPredictionId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data UpdateBatchPrediction = UpdateBatchPrediction
    { _ubpBatchPredictionId   :: Text
    , _ubpBatchPredictionName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateBatchPrediction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubpBatchPredictionId' @::@ 'Text'
--
-- * 'ubpBatchPredictionName' @::@ 'Text'
--
updateBatchPrediction :: Text -- ^ 'ubpBatchPredictionId'
                      -> Text -- ^ 'ubpBatchPredictionName'
                      -> UpdateBatchPrediction
updateBatchPrediction p1 p2 = UpdateBatchPrediction
    { _ubpBatchPredictionId   = p1
    , _ubpBatchPredictionName = p2
    }

-- | The ID assigned to the 'BatchPrediction' during creation.
ubpBatchPredictionId :: Lens' UpdateBatchPrediction Text
ubpBatchPredictionId =
    lens _ubpBatchPredictionId (\s a -> s { _ubpBatchPredictionId = a })

-- | A new user-supplied name or description of the 'BatchPrediction'.
ubpBatchPredictionName :: Lens' UpdateBatchPrediction Text
ubpBatchPredictionName =
    lens _ubpBatchPredictionName (\s a -> s { _ubpBatchPredictionName = a })

newtype UpdateBatchPredictionResponse = UpdateBatchPredictionResponse
    { _ubprBatchPredictionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UpdateBatchPredictionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubprBatchPredictionId' @::@ 'Maybe' 'Text'
--
updateBatchPredictionResponse :: UpdateBatchPredictionResponse
updateBatchPredictionResponse = UpdateBatchPredictionResponse
    { _ubprBatchPredictionId = Nothing
    }

-- | The ID assigned to the 'BatchPrediction' during creation. This value should be
-- identical to the value of the 'BatchPredictionId' in the request.
ubprBatchPredictionId :: Lens' UpdateBatchPredictionResponse (Maybe Text)
ubprBatchPredictionId =
    lens _ubprBatchPredictionId (\s a -> s { _ubprBatchPredictionId = a })

instance ToPath UpdateBatchPrediction where
    toPath = const "/"

instance ToQuery UpdateBatchPrediction where
    toQuery = const mempty

instance ToHeaders UpdateBatchPrediction

instance ToJSON UpdateBatchPrediction where
    toJSON UpdateBatchPrediction{..} = object
        [ "BatchPredictionId"   .= _ubpBatchPredictionId
        , "BatchPredictionName" .= _ubpBatchPredictionName
        ]

instance AWSRequest UpdateBatchPrediction where
    type Sv UpdateBatchPrediction = MachineLearning
    type Rs UpdateBatchPrediction = UpdateBatchPredictionResponse

    request  = post "UpdateBatchPrediction"
    response = jsonResponse

instance FromJSON UpdateBatchPredictionResponse where
    parseJSON = withObject "UpdateBatchPredictionResponse" $ \o -> UpdateBatchPredictionResponse
        <$> o .:? "BatchPredictionId"
