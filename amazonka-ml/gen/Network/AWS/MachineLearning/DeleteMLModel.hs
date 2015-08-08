{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to an @MLModel@, rendering it unusable.
--
-- After using the @DeleteMLModel@ operation, you can use the GetMLModel
-- operation to verify that the status of the @MLModel@ changed to DELETED.
--
-- Caution
--
-- The result of the @DeleteMLModel@ operation is irreversible.
--
-- /See:/ <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteMLModel.html AWS API Reference> for DeleteMLModel.
module Network.AWS.MachineLearning.DeleteMLModel
    (
    -- * Creating a Request
      DeleteMLModel
    , deleteMLModel
    -- * Request Lenses
    , dmlmMLModelId

    -- * Destructuring the Response
    , DeleteMLModelResponse
    , deleteMLModelResponse
    -- * Response Lenses
    , dmlmrsMLModelId
    , dmlmrsStatus
    ) where

import           Network.AWS.MachineLearning.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteMLModel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmMLModelId'
newtype DeleteMLModel = DeleteMLModel'
    { _dmlmMLModelId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMLModel' smart constructor.
deleteMLModel :: Text -> DeleteMLModel
deleteMLModel pMLModelId_ =
    DeleteMLModel'
    { _dmlmMLModelId = pMLModelId_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@.
dmlmMLModelId :: Lens' DeleteMLModel Text
dmlmMLModelId = lens _dmlmMLModelId (\ s a -> s{_dmlmMLModelId = a});

instance AWSRequest DeleteMLModel where
        type Sv DeleteMLModel = MachineLearning
        type Rs DeleteMLModel = DeleteMLModelResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteMLModelResponse' <$>
                   (x .?> "MLModelId") <*> (pure (fromEnum s)))

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
          = object ["MLModelId" .= _dmlmMLModelId]

instance ToPath DeleteMLModel where
        toPath = const "/"

instance ToQuery DeleteMLModel where
        toQuery = const mempty

-- | Represents the output of a DeleteMLModel operation.
--
-- You can use the GetMLModel operation and check the value of the @Status@
-- parameter to see whether an @MLModel@ is marked as @DELETED@.
--
-- /See:/ 'deleteMLModelResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmlmrsMLModelId'
--
-- * 'dmlmrsStatus'
data DeleteMLModelResponse = DeleteMLModelResponse'
    { _dmlmrsMLModelId :: !(Maybe Text)
    , _dmlmrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMLModelResponse' smart constructor.
deleteMLModelResponse :: Int -> DeleteMLModelResponse
deleteMLModelResponse pStatus_ =
    DeleteMLModelResponse'
    { _dmlmrsMLModelId = Nothing
    , _dmlmrsStatus = pStatus_
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelID@ in the request.
dmlmrsMLModelId :: Lens' DeleteMLModelResponse (Maybe Text)
dmlmrsMLModelId = lens _dmlmrsMLModelId (\ s a -> s{_dmlmrsMLModelId = a});

-- | Undocumented member.
dmlmrsStatus :: Lens' DeleteMLModelResponse Int
dmlmrsStatus = lens _dmlmrsStatus (\ s a -> s{_dmlmrsStatus = a});
