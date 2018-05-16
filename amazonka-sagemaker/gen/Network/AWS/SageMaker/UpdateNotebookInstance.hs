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
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance. NotebookInstance updates include upgrading or downgrading the ML compute instance used for your notebook instance to accommodate changes in your workload requirements. You can also update the VPC security groups.
--
--
module Network.AWS.SageMaker.UpdateNotebookInstance
    (
    -- * Creating a Request
      updateNotebookInstance
    , UpdateNotebookInstance
    -- * Request Lenses
    , uniInstanceType
    , uniRoleARN
    , uniNotebookInstanceName

    -- * Destructuring the Response
    , updateNotebookInstanceResponse
    , UpdateNotebookInstanceResponse
    -- * Response Lenses
    , unirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'updateNotebookInstance' smart constructor.
data UpdateNotebookInstance = UpdateNotebookInstance'
  { _uniInstanceType         :: !(Maybe InstanceType)
  , _uniRoleARN              :: !(Maybe Text)
  , _uniNotebookInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotebookInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uniInstanceType' - The Amazon ML compute instance type.
--
-- * 'uniRoleARN' - Amazon Resource Name (ARN) of the IAM role to associate with the instance.
--
-- * 'uniNotebookInstanceName' - The name of the notebook instance to update.
updateNotebookInstance
    :: Text -- ^ 'uniNotebookInstanceName'
    -> UpdateNotebookInstance
updateNotebookInstance pNotebookInstanceName_ =
  UpdateNotebookInstance'
    { _uniInstanceType = Nothing
    , _uniRoleARN = Nothing
    , _uniNotebookInstanceName = pNotebookInstanceName_
    }


-- | The Amazon ML compute instance type.
uniInstanceType :: Lens' UpdateNotebookInstance (Maybe InstanceType)
uniInstanceType = lens _uniInstanceType (\ s a -> s{_uniInstanceType = a})

-- | Amazon Resource Name (ARN) of the IAM role to associate with the instance.
uniRoleARN :: Lens' UpdateNotebookInstance (Maybe Text)
uniRoleARN = lens _uniRoleARN (\ s a -> s{_uniRoleARN = a})

-- | The name of the notebook instance to update.
uniNotebookInstanceName :: Lens' UpdateNotebookInstance Text
uniNotebookInstanceName = lens _uniNotebookInstanceName (\ s a -> s{_uniNotebookInstanceName = a})

instance AWSRequest UpdateNotebookInstance where
        type Rs UpdateNotebookInstance =
             UpdateNotebookInstanceResponse
        request = postJSON sageMaker
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateNotebookInstanceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateNotebookInstance where

instance NFData UpdateNotebookInstance where

instance ToHeaders UpdateNotebookInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.UpdateNotebookInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNotebookInstance where
        toJSON UpdateNotebookInstance'{..}
          = object
              (catMaybes
                 [("InstanceType" .=) <$> _uniInstanceType,
                  ("RoleArn" .=) <$> _uniRoleARN,
                  Just
                    ("NotebookInstanceName" .=
                       _uniNotebookInstanceName)])

instance ToPath UpdateNotebookInstance where
        toPath = const "/"

instance ToQuery UpdateNotebookInstance where
        toQuery = const mempty

-- | /See:/ 'updateNotebookInstanceResponse' smart constructor.
newtype UpdateNotebookInstanceResponse = UpdateNotebookInstanceResponse'
  { _unirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unirsResponseStatus' - -- | The response status code.
updateNotebookInstanceResponse
    :: Int -- ^ 'unirsResponseStatus'
    -> UpdateNotebookInstanceResponse
updateNotebookInstanceResponse pResponseStatus_ =
  UpdateNotebookInstanceResponse' {_unirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
unirsResponseStatus :: Lens' UpdateNotebookInstanceResponse Int
unirsResponseStatus = lens _unirsResponseStatus (\ s a -> s{_unirsResponseStatus = a})

instance NFData UpdateNotebookInstanceResponse where
