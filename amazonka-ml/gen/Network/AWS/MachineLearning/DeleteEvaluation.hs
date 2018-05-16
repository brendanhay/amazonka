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
-- Module      : Network.AWS.MachineLearning.DeleteEvaluation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @Evaluation@ , rendering it unusable.
--
--
-- After invoking the @DeleteEvaluation@ operation, you can use the @GetEvaluation@ operation to verify that the status of the @Evaluation@ changed to @DELETED@ .
--
-- ____Caution__
-- The results of the @DeleteEvaluation@ operation are irreversible.
--
-- __
module Network.AWS.MachineLearning.DeleteEvaluation
    (
    -- * Creating a Request
      deleteEvaluation
    , DeleteEvaluation
    -- * Request Lenses
    , deEvaluationId

    -- * Destructuring the Response
    , deleteEvaluationResponse
    , DeleteEvaluationResponse
    -- * Response Lenses
    , dersEvaluationId
    , dersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEvaluation' smart constructor.
newtype DeleteEvaluation = DeleteEvaluation'
  { _deEvaluationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEvaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEvaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
deleteEvaluation
    :: Text -- ^ 'deEvaluationId'
    -> DeleteEvaluation
deleteEvaluation pEvaluationId_ =
  DeleteEvaluation' {_deEvaluationId = pEvaluationId_}


-- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
deEvaluationId :: Lens' DeleteEvaluation Text
deEvaluationId = lens _deEvaluationId (\ s a -> s{_deEvaluationId = a})

instance AWSRequest DeleteEvaluation where
        type Rs DeleteEvaluation = DeleteEvaluationResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DeleteEvaluationResponse' <$>
                   (x .?> "EvaluationId") <*> (pure (fromEnum s)))

instance Hashable DeleteEvaluation where

instance NFData DeleteEvaluation where

instance ToHeaders DeleteEvaluation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteEvaluation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEvaluation where
        toJSON DeleteEvaluation'{..}
          = object
              (catMaybes
                 [Just ("EvaluationId" .= _deEvaluationId)])

instance ToPath DeleteEvaluation where
        toPath = const "/"

instance ToQuery DeleteEvaluation where
        toQuery = const mempty

-- | Represents the output of a @DeleteEvaluation@ operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.
--
--
-- You can use the @GetEvaluation@ operation and check the value of the @Status@ parameter to see whether an @Evaluation@ is marked as @DELETED@ .
--
--
-- /See:/ 'deleteEvaluationResponse' smart constructor.
data DeleteEvaluationResponse = DeleteEvaluationResponse'
  { _dersEvaluationId   :: !(Maybe Text)
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEvaluationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersEvaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
--
-- * 'dersResponseStatus' - -- | The response status code.
deleteEvaluationResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DeleteEvaluationResponse
deleteEvaluationResponse pResponseStatus_ =
  DeleteEvaluationResponse'
    {_dersEvaluationId = Nothing, _dersResponseStatus = pResponseStatus_}


-- | A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
dersEvaluationId :: Lens' DeleteEvaluationResponse (Maybe Text)
dersEvaluationId = lens _dersEvaluationId (\ s a -> s{_dersEvaluationId = a})

-- | -- | The response status code.
dersResponseStatus :: Lens' DeleteEvaluationResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DeleteEvaluationResponse where
