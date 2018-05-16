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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @EvaluationName@ of an @Evaluation@ .
--
--
-- You can use the @GetEvaluation@ operation to view the contents of the updated data element.
--
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
    , uersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEvaluation' smart constructor.
data UpdateEvaluation = UpdateEvaluation'
  { _ueEvaluationId   :: !Text
  , _ueEvaluationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueEvaluationId' - The ID assigned to the @Evaluation@ during creation.
--
-- * 'ueEvaluationName' - A new user-supplied name or description of the @Evaluation@ that will replace the current content.
updateEvaluation
    :: Text -- ^ 'ueEvaluationId'
    -> Text -- ^ 'ueEvaluationName'
    -> UpdateEvaluation
updateEvaluation pEvaluationId_ pEvaluationName_ =
  UpdateEvaluation'
    {_ueEvaluationId = pEvaluationId_, _ueEvaluationName = pEvaluationName_}


-- | The ID assigned to the @Evaluation@ during creation.
ueEvaluationId :: Lens' UpdateEvaluation Text
ueEvaluationId = lens _ueEvaluationId (\ s a -> s{_ueEvaluationId = a})

-- | A new user-supplied name or description of the @Evaluation@ that will replace the current content.
ueEvaluationName :: Lens' UpdateEvaluation Text
ueEvaluationName = lens _ueEvaluationName (\ s a -> s{_ueEvaluationName = a})

instance AWSRequest UpdateEvaluation where
        type Rs UpdateEvaluation = UpdateEvaluationResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEvaluationResponse' <$>
                   (x .?> "EvaluationId") <*> (pure (fromEnum s)))

instance Hashable UpdateEvaluation where

instance NFData UpdateEvaluation where

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

-- | Represents the output of an @UpdateEvaluation@ operation.
--
--
-- You can see the updated content by using the @GetEvaluation@ operation.
--
--
-- /See:/ 'updateEvaluationResponse' smart constructor.
data UpdateEvaluationResponse = UpdateEvaluationResponse'
  { _uersEvaluationId   :: !(Maybe Text)
  , _uersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uersEvaluationId' - The ID assigned to the @Evaluation@ during creation. This value should be identical to the value of the @Evaluation@ in the request.
--
-- * 'uersResponseStatus' - -- | The response status code.
updateEvaluationResponse
    :: Int -- ^ 'uersResponseStatus'
    -> UpdateEvaluationResponse
updateEvaluationResponse pResponseStatus_ =
  UpdateEvaluationResponse'
    {_uersEvaluationId = Nothing, _uersResponseStatus = pResponseStatus_}


-- | The ID assigned to the @Evaluation@ during creation. This value should be identical to the value of the @Evaluation@ in the request.
uersEvaluationId :: Lens' UpdateEvaluationResponse (Maybe Text)
uersEvaluationId = lens _uersEvaluationId (\ s a -> s{_uersEvaluationId = a})

-- | -- | The response status code.
uersResponseStatus :: Lens' UpdateEvaluationResponse Int
uersResponseStatus = lens _uersResponseStatus (\ s a -> s{_uersResponseStatus = a})

instance NFData UpdateEvaluationResponse where
