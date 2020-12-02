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
-- Module      : Network.AWS.SageMaker.DeleteModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model. The @DeleteModel@ API deletes only the model entry that was created in Amazon SageMaker when you called the <http://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateModel.html CreateModel> API. It does not delete model artifacts, inference code, or the IAM role that you specified when creating the model.
--
--
module Network.AWS.SageMaker.DeleteModel
    (
    -- * Creating a Request
      deleteModel
    , DeleteModel
    -- * Request Lenses
    , dmModelName

    -- * Destructuring the Response
    , deleteModelResponse
    , DeleteModelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'deleteModel' smart constructor.
newtype DeleteModel = DeleteModel'
  { _dmModelName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmModelName' - The name of the model to delete.
deleteModel
    :: Text -- ^ 'dmModelName'
    -> DeleteModel
deleteModel pModelName_ = DeleteModel' {_dmModelName = pModelName_}


-- | The name of the model to delete.
dmModelName :: Lens' DeleteModel Text
dmModelName = lens _dmModelName (\ s a -> s{_dmModelName = a})

instance AWSRequest DeleteModel where
        type Rs DeleteModel = DeleteModelResponse
        request = postJSON sageMaker
        response = receiveNull DeleteModelResponse'

instance Hashable DeleteModel where

instance NFData DeleteModel where

instance ToHeaders DeleteModel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DeleteModel" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteModel where
        toJSON DeleteModel'{..}
          = object
              (catMaybes [Just ("ModelName" .= _dmModelName)])

instance ToPath DeleteModel where
        toPath = const "/"

instance ToQuery DeleteModel where
        toQuery = const mempty

-- | /See:/ 'deleteModelResponse' smart constructor.
data DeleteModelResponse =
  DeleteModelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteModelResponse' with the minimum fields required to make a request.
--
deleteModelResponse
    :: DeleteModelResponse
deleteModelResponse = DeleteModelResponse'


instance NFData DeleteModelResponse where
