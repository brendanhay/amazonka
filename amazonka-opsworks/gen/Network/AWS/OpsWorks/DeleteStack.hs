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
-- Module      : Network.AWS.OpsWorks.DeleteStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified stack. You must first delete all instances, layers, and apps or deregister registered instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html Shut Down a Stack> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeleteStack
    (
    -- * Creating a Request
      deleteStack
    , DeleteStack
    -- * Request Lenses
    , dsStackId

    -- * Destructuring the Response
    , deleteStackResponse
    , DeleteStackResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStack' smart constructor.
newtype DeleteStack = DeleteStack'
  { _dsStackId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsStackId' - The stack ID.
deleteStack
    :: Text -- ^ 'dsStackId'
    -> DeleteStack
deleteStack pStackId_ = DeleteStack' {_dsStackId = pStackId_}


-- | The stack ID.
dsStackId :: Lens' DeleteStack Text
dsStackId = lens _dsStackId (\ s a -> s{_dsStackId = a})

instance AWSRequest DeleteStack where
        type Rs DeleteStack = DeleteStackResponse
        request = postJSON opsWorks
        response = receiveNull DeleteStackResponse'

instance Hashable DeleteStack where

instance NFData DeleteStack where

instance ToHeaders DeleteStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeleteStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteStack where
        toJSON DeleteStack'{..}
          = object (catMaybes [Just ("StackId" .= _dsStackId)])

instance ToPath DeleteStack where
        toPath = const "/"

instance ToQuery DeleteStack where
        toQuery = const mempty

-- | /See:/ 'deleteStackResponse' smart constructor.
data DeleteStackResponse =
  DeleteStackResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStackResponse' with the minimum fields required to make a request.
--
deleteStackResponse
    :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse'


instance NFData DeleteStackResponse where
