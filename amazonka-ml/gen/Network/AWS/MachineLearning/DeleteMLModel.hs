{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Assigns the DELETED status to an @MLModel@, rendering it unusable.
--
-- After using the @DeleteMLModel@ operation, you can use the GetMLModel
-- operation to verify that the status of the @MLModel@ changed to DELETED.
--
-- Caution
--
-- The result of the @DeleteMLModel@ operation is irreversible.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteMLModel.html>
module Network.AWS.MachineLearning.DeleteMLModel
    (
    -- * Request
      DeleteMLModel
    -- ** Request constructor
    , deleteMLModel
    -- ** Request lenses
    , dmlmMLModelId

    -- * Response
    , DeleteMLModelResponse
    -- ** Response constructor
    , deleteMLModelResponse
    -- ** Response lenses
    , dmlmrMLModelId
    , dmlmrStatus
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
    } deriving (Eq,Read,Show)

-- | 'DeleteMLModel' smart constructor.
deleteMLModel :: Text -> DeleteMLModel
deleteMLModel pMLModelId =
    DeleteMLModel'
    { _dmlmMLModelId = pMLModelId
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
                   (x .?> "MLModelId") <*> (pure s))

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
-- * 'dmlmrMLModelId'
--
-- * 'dmlmrStatus'
data DeleteMLModelResponse = DeleteMLModelResponse'
    { _dmlmrMLModelId :: !(Maybe Text)
    , _dmlmrStatus    :: !Status
    } deriving (Eq,Show)

-- | 'DeleteMLModelResponse' smart constructor.
deleteMLModelResponse :: Status -> DeleteMLModelResponse
deleteMLModelResponse pStatus =
    DeleteMLModelResponse'
    { _dmlmrMLModelId = Nothing
    , _dmlmrStatus = pStatus
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@. This value
-- should be identical to the value of the @MLModelID@ in the request.
dmlmrMLModelId :: Lens' DeleteMLModelResponse (Maybe Text)
dmlmrMLModelId = lens _dmlmrMLModelId (\ s a -> s{_dmlmrMLModelId = a});

-- | FIXME: Undocumented member.
dmlmrStatus :: Lens' DeleteMLModelResponse Status
dmlmrStatus = lens _dmlmrStatus (\ s a -> s{_dmlmrStatus = a});
