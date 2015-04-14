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

-- Module      : Network.AWS.MachineLearning.DeleteEvaluation
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

-- | Assigns the 'DELETED' status to an 'Evaluation', rendering it unusable.
--
-- After invoking the 'DeleteEvaluation' operation, you can use the 'GetEvaluation'
-- operation to verify that the status of the 'Evaluation' changed to 'DELETED'.
--
-- Caution The results of the 'DeleteEvaluation' operation are irreversible.
--
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteEvaluation.html>
module Network.AWS.MachineLearning.DeleteEvaluation
    (
    -- * Request
      DeleteEvaluation
    -- ** Request constructor
    , deleteEvaluation
    -- ** Request lenses
    , deEvaluationId

    -- * Response
    , DeleteEvaluationResponse
    -- ** Response constructor
    , deleteEvaluationResponse
    -- ** Response lenses
    , derEvaluationId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype DeleteEvaluation = DeleteEvaluation
    { _deEvaluationId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteEvaluation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deEvaluationId' @::@ 'Text'
--
deleteEvaluation :: Text -- ^ 'deEvaluationId'
                 -> DeleteEvaluation
deleteEvaluation p1 = DeleteEvaluation
    { _deEvaluationId = p1
    }

-- | A user-supplied ID that uniquely identifies the 'Evaluation' to delete.
deEvaluationId :: Lens' DeleteEvaluation Text
deEvaluationId = lens _deEvaluationId (\s a -> s { _deEvaluationId = a })

newtype DeleteEvaluationResponse = DeleteEvaluationResponse
    { _derEvaluationId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DeleteEvaluationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEvaluationId' @::@ 'Maybe' 'Text'
--
deleteEvaluationResponse :: DeleteEvaluationResponse
deleteEvaluationResponse = DeleteEvaluationResponse
    { _derEvaluationId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'Evaluation'. This value should
-- be identical to the value of the 'EvaluationId' in the request.
derEvaluationId :: Lens' DeleteEvaluationResponse (Maybe Text)
derEvaluationId = lens _derEvaluationId (\s a -> s { _derEvaluationId = a })

instance ToPath DeleteEvaluation where
    toPath = const "/"

instance ToQuery DeleteEvaluation where
    toQuery = const mempty

instance ToHeaders DeleteEvaluation

instance ToJSON DeleteEvaluation where
    toJSON DeleteEvaluation{..} = object
        [ "EvaluationId" .= _deEvaluationId
        ]

instance AWSRequest DeleteEvaluation where
    type Sv DeleteEvaluation = MachineLearning
    type Rs DeleteEvaluation = DeleteEvaluationResponse

    request  = post "DeleteEvaluation"
    response = jsonResponse

instance FromJSON DeleteEvaluationResponse where
    parseJSON = withObject "DeleteEvaluationResponse" $ \o -> DeleteEvaluationResponse
        <$> o .:? "EvaluationId"
