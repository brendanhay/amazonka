{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deletes a specified stack. You must first delete all instances, layers,
-- and apps or deregister registered instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html Shut Down a Stack>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteStack.html>
module Network.AWS.OpsWorks.DeleteStack
    (
    -- * Request
      DeleteStack
    -- ** Request constructor
    , deleteStack
    -- ** Request lenses
    , dsStackId

    -- * Response
    , DeleteStackResponse
    -- ** Response constructor
    , deleteStackResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStackId'
newtype DeleteStack = DeleteStack'
    { _dsStackId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteStack' smart constructor.
deleteStack :: Text -> DeleteStack
deleteStack pStackId =
    DeleteStack'
    { _dsStackId = pStackId
    }

-- | The stack ID.
dsStackId :: Lens' DeleteStack Text
dsStackId = lens _dsStackId (\ s a -> s{_dsStackId = a});

instance AWSRequest DeleteStack where
        type Sv DeleteStack = OpsWorks
        type Rs DeleteStack = DeleteStackResponse
        request = postJSON
        response = receiveNull DeleteStackResponse'

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
          = object ["StackId" .= _dsStackId]

instance ToPath DeleteStack where
        toPath = const "/"

instance ToQuery DeleteStack where
        toQuery = const mempty

-- | /See:/ 'deleteStackResponse' smart constructor.
data DeleteStackResponse =
    DeleteStackResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteStackResponse' smart constructor.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse'
