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

-- Module      : Network.AWS.MachineLearning.UpdateEvaluation
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

-- | Updates the 'EvaluationName' of an 'Evaluation'.
--
-- You can use the 'GetEvaluation' operation to view the contents of the updated
-- data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateEvaluation.html>
module Network.AWS.MachineLearning.UpdateEvaluation
    (
    -- * Request
      UpdateEvaluation
    -- ** Request constructor
    , updateEvaluation
    -- ** Request lenses
    , ueEvaluationId
    , ueEvaluationName

    -- * Response
    , UpdateEvaluationResponse
    -- ** Response constructor
    , updateEvaluationResponse
    -- ** Response lenses
    , uerEvaluationId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data UpdateEvaluation = UpdateEvaluation
    { _ueEvaluationId   :: Text
    , _ueEvaluationName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateEvaluation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ueEvaluationId' @::@ 'Text'
--
-- * 'ueEvaluationName' @::@ 'Text'
--
updateEvaluation :: Text -- ^ 'ueEvaluationId'
                 -> Text -- ^ 'ueEvaluationName'
                 -> UpdateEvaluation
updateEvaluation p1 p2 = UpdateEvaluation
    { _ueEvaluationId   = p1
    , _ueEvaluationName = p2
    }

-- | The ID assigned to the 'Evaluation' during creation.
ueEvaluationId :: Lens' UpdateEvaluation Text
ueEvaluationId = lens _ueEvaluationId (\s a -> s { _ueEvaluationId = a })

-- | A new user-supplied name or description of the 'Evaluation' that will replace
-- the current content.
ueEvaluationName :: Lens' UpdateEvaluation Text
ueEvaluationName = lens _ueEvaluationName (\s a -> s { _ueEvaluationName = a })

newtype UpdateEvaluationResponse = UpdateEvaluationResponse
    { _uerEvaluationId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UpdateEvaluationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uerEvaluationId' @::@ 'Maybe' 'Text'
--
updateEvaluationResponse :: UpdateEvaluationResponse
updateEvaluationResponse = UpdateEvaluationResponse
    { _uerEvaluationId = Nothing
    }

-- | The ID assigned to the 'Evaluation' during creation. This value should be
-- identical to the value of the 'Evaluation' in the request.
uerEvaluationId :: Lens' UpdateEvaluationResponse (Maybe Text)
uerEvaluationId = lens _uerEvaluationId (\s a -> s { _uerEvaluationId = a })

instance ToPath UpdateEvaluation where
    toPath = const "/"

instance ToQuery UpdateEvaluation where
    toQuery = const mempty

instance ToHeaders UpdateEvaluation

instance ToJSON UpdateEvaluation where
    toJSON UpdateEvaluation{..} = object
        [ "EvaluationId"   .= _ueEvaluationId
        , "EvaluationName" .= _ueEvaluationName
        ]

instance AWSRequest UpdateEvaluation where
    type Sv UpdateEvaluation = MachineLearning
    type Rs UpdateEvaluation = UpdateEvaluationResponse

    request  = post "UpdateEvaluation"
    response = jsonResponse

instance FromJSON UpdateEvaluationResponse where
    parseJSON = withObject "UpdateEvaluationResponse" $ \o -> UpdateEvaluationResponse
        <$> o .:? "EvaluationId"
