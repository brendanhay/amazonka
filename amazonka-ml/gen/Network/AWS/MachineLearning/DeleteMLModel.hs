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
-- Module      : Network.AWS.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @MLModel@ , rendering it unusable.
--
--
-- After using the @DeleteMLModel@ operation, you can use the @GetMLModel@ operation to verify that the status of the @MLModel@ changed to DELETED.
--
-- __Caution:__ The result of the @DeleteMLModel@ operation is irreversible.
--
module Network.AWS.MachineLearning.DeleteMLModel
    (
    -- * Creating a Request
      deleteMLModel
    , DeleteMLModel
    -- * Request Lenses
    , dmlmMLModelId

    -- * Destructuring the Response
    , deleteMLModelResponse
    , DeleteMLModelResponse
    -- * Response Lenses
    , dmlmrsMLModelId
    , dmlmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMLModel' smart constructor.
newtype DeleteMLModel = DeleteMLModel'
  { _dmlmMLModelId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMLModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlmMLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ .
deleteMLModel
    :: Text -- ^ 'dmlmMLModelId'
    -> DeleteMLModel
deleteMLModel pMLModelId_ = DeleteMLModel' {_dmlmMLModelId = pMLModelId_}


-- | A user-supplied ID that uniquely identifies the @MLModel@ .
dmlmMLModelId :: Lens' DeleteMLModel Text
dmlmMLModelId = lens _dmlmMLModelId (\ s a -> s{_dmlmMLModelId = a})

instance AWSRequest DeleteMLModel where
        type Rs DeleteMLModel = DeleteMLModelResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DeleteMLModelResponse' <$>
                   (x .?> "MLModelId") <*> (pure (fromEnum s)))

instance Hashable DeleteMLModel where

instance NFData DeleteMLModel where

instance ToHeaders DeleteMLModel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DeleteMLModel" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMLModel where
        toJSON DeleteMLModel'{..}
          = object
              (catMaybes [Just ("MLModelId" .= _dmlmMLModelId)])

instance ToPath DeleteMLModel where
        toPath = const "/"

instance ToQuery DeleteMLModel where
        toQuery = const mempty

-- | Represents the output of a @DeleteMLModel@ operation.
--
--
-- You can use the @GetMLModel@ operation and check the value of the @Status@ parameter to see whether an @MLModel@ is marked as @DELETED@ .
--
--
-- /See:/ 'deleteMLModelResponse' smart constructor.
data DeleteMLModelResponse = DeleteMLModelResponse'
  { _dmlmrsMLModelId      :: !(Maybe Text)
  , _dmlmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMLModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmlmrsMLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelID@ in the request.
--
-- * 'dmlmrsResponseStatus' - -- | The response status code.
deleteMLModelResponse
    :: Int -- ^ 'dmlmrsResponseStatus'
    -> DeleteMLModelResponse
deleteMLModelResponse pResponseStatus_ =
  DeleteMLModelResponse'
    {_dmlmrsMLModelId = Nothing, _dmlmrsResponseStatus = pResponseStatus_}


-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelID@ in the request.
dmlmrsMLModelId :: Lens' DeleteMLModelResponse (Maybe Text)
dmlmrsMLModelId = lens _dmlmrsMLModelId (\ s a -> s{_dmlmrsMLModelId = a})

-- | -- | The response status code.
dmlmrsResponseStatus :: Lens' DeleteMLModelResponse Int
dmlmrsResponseStatus = lens _dmlmrsResponseStatus (\ s a -> s{_dmlmrsResponseStatus = a})

instance NFData DeleteMLModelResponse where
