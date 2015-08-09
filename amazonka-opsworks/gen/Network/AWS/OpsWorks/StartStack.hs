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
-- Module      : Network.AWS.OpsWorks.StartStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a stack\'s instances.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_StartStack.html AWS API Reference> for StartStack.
module Network.AWS.OpsWorks.StartStack
    (
    -- * Creating a Request
      StartStack
    , startStack
    -- * Request Lenses
    , staStackId

    -- * Destructuring the Response
    , StartStackResponse
    , startStackResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staStackId'
newtype StartStack = StartStack'
    { _staStackId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartStack' smart constructor.
startStack :: Text -> StartStack
startStack pStackId_ =
    StartStack'
    { _staStackId = pStackId_
    }

-- | The stack ID.
staStackId :: Lens' StartStack Text
staStackId = lens _staStackId (\ s a -> s{_staStackId = a});

instance AWSRequest StartStack where
        type Sv StartStack = OpsWorks
        type Rs StartStack = StartStackResponse
        request = postJSON
        response = receiveNull StartStackResponse'

instance ToHeaders StartStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.StartStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartStack where
        toJSON StartStack'{..}
          = object ["StackId" .= _staStackId]

instance ToPath StartStack where
        toPath = const "/"

instance ToQuery StartStack where
        toQuery = const mempty

-- | /See:/ 'startStackResponse' smart constructor.
data StartStackResponse =
    StartStackResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StartStackResponse' smart constructor.
startStackResponse :: StartStackResponse
startStackResponse = StartStackResponse'
