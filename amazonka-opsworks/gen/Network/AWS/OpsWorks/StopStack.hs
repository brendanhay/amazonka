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
-- Module      : Network.AWS.OpsWorks.StopStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified stack.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.StopStack
    (
    -- * Creating a Request
      stopStack
    , StopStack
    -- * Request Lenses
    , stoStackId

    -- * Destructuring the Response
    , stopStackResponse
    , StopStackResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopStack' smart constructor.
newtype StopStack = StopStack'
  { _stoStackId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stoStackId' - The stack ID.
stopStack
    :: Text -- ^ 'stoStackId'
    -> StopStack
stopStack pStackId_ = StopStack' {_stoStackId = pStackId_}


-- | The stack ID.
stoStackId :: Lens' StopStack Text
stoStackId = lens _stoStackId (\ s a -> s{_stoStackId = a})

instance AWSRequest StopStack where
        type Rs StopStack = StopStackResponse
        request = postJSON opsWorks
        response = receiveNull StopStackResponse'

instance Hashable StopStack where

instance NFData StopStack where

instance ToHeaders StopStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.StopStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopStack where
        toJSON StopStack'{..}
          = object
              (catMaybes [Just ("StackId" .= _stoStackId)])

instance ToPath StopStack where
        toPath = const "/"

instance ToQuery StopStack where
        toQuery = const mempty

-- | /See:/ 'stopStackResponse' smart constructor.
data StopStackResponse =
  StopStackResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopStackResponse' with the minimum fields required to make a request.
--
stopStackResponse
    :: StopStackResponse
stopStackResponse = StopStackResponse'


instance NFData StopStackResponse where
