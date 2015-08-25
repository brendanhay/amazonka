{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateEvaluation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the 'EvaluationName' of an 'Evaluation'.
--
-- You can use the GetEvaluation operation to view the contents of the
-- updated data element.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateEvaluation.html AWS API Reference> for UpdateEvaluation.
module Network.AWS.MachineLearning.UpdateEvaluation
    (
    -- * Creating a Request
      updateEvaluation
    , UpdateEvaluation
    -- * Request Lenses
    , ueEvaluationId
    , ueEvaluationName

    -- * Destructuring the Response
    , updateEvaluationResponse
    , UpdateEvaluationResponse
    -- * Response Lenses
    , uersEvaluationId
    , uersStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateEvaluation' smart constructor.
data UpdateEvaluation = UpdateEvaluation'
    { _ueEvaluationId   :: !Text
    , _ueEvaluationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueEvaluationId'
--
-- * 'ueEvaluationName'
updateEvaluation
    :: Text -- ^ 'ueEvaluationId'
    -> Text -- ^ 'ueEvaluationName'
    -> UpdateEvaluation
updateEvaluation pEvaluationId_ pEvaluationName_ =
    UpdateEvaluation'
    { _ueEvaluationId = pEvaluationId_
    , _ueEvaluationName = pEvaluationName_
    }

-- | The ID assigned to the 'Evaluation' during creation.
ueEvaluationId :: Lens' UpdateEvaluation Text
ueEvaluationId = lens _ueEvaluationId (\ s a -> s{_ueEvaluationId = a});

-- | A new user-supplied name or description of the 'Evaluation' that will
-- replace the current content.
ueEvaluationName :: Lens' UpdateEvaluation Text
ueEvaluationName = lens _ueEvaluationName (\ s a -> s{_ueEvaluationName = a});

instance AWSRequest UpdateEvaluation where
        type Rs UpdateEvaluation = UpdateEvaluationResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEvaluationResponse' <$>
                   (x .?> "EvaluationId") <*> (pure (fromEnum s)))

instance ToHeaders UpdateEvaluation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.UpdateEvaluation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEvaluation where
        toJSON UpdateEvaluation'{..}
          = object
              (catMaybes
                 [Just ("EvaluationId" .= _ueEvaluationId),
                  Just ("EvaluationName" .= _ueEvaluationName)])

instance ToPath UpdateEvaluation where
        toPath = const "/"

instance ToQuery UpdateEvaluation where
        toQuery = const mempty

-- | Represents the output of an UpdateEvaluation operation.
--
-- You can see the updated content by using the GetEvaluation operation.
--
-- /See:/ 'updateEvaluationResponse' smart constructor.
data UpdateEvaluationResponse = UpdateEvaluationResponse'
    { _uersEvaluationId :: !(Maybe Text)
    , _uersStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uersEvaluationId'
--
-- * 'uersStatus'
updateEvaluationResponse
    :: Int -- ^ 'uersStatus'
    -> UpdateEvaluationResponse
updateEvaluationResponse pStatus_ =
    UpdateEvaluationResponse'
    { _uersEvaluationId = Nothing
    , _uersStatus = pStatus_
    }

-- | The ID assigned to the 'Evaluation' during creation. This value should
-- be identical to the value of the 'Evaluation' in the request.
uersEvaluationId :: Lens' UpdateEvaluationResponse (Maybe Text)
uersEvaluationId = lens _uersEvaluationId (\ s a -> s{_uersEvaluationId = a});

-- | The response status code.
uersStatus :: Lens' UpdateEvaluationResponse Int
uersStatus = lens _uersStatus (\ s a -> s{_uersStatus = a});
