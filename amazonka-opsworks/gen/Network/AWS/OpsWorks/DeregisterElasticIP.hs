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
-- Module      : Network.AWS.OpsWorks.DeregisterElasticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Elastic IP address. The address can then be registered by another stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DeregisterElasticIP
    (
    -- * Creating a Request
      deregisterElasticIP
    , DeregisterElasticIP
    -- * Request Lenses
    , deipElasticIP

    -- * Destructuring the Response
    , deregisterElasticIPResponse
    , DeregisterElasticIPResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterElasticIP' smart constructor.
newtype DeregisterElasticIP = DeregisterElasticIP'
  { _deipElasticIP :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterElasticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deipElasticIP' - The Elastic IP address.
deregisterElasticIP
    :: Text -- ^ 'deipElasticIP'
    -> DeregisterElasticIP
deregisterElasticIP pElasticIP_ =
  DeregisterElasticIP' {_deipElasticIP = pElasticIP_}


-- | The Elastic IP address.
deipElasticIP :: Lens' DeregisterElasticIP Text
deipElasticIP = lens _deipElasticIP (\ s a -> s{_deipElasticIP = a})

instance AWSRequest DeregisterElasticIP where
        type Rs DeregisterElasticIP =
             DeregisterElasticIPResponse
        request = postJSON opsWorks
        response = receiveNull DeregisterElasticIPResponse'

instance Hashable DeregisterElasticIP where

instance NFData DeregisterElasticIP where

instance ToHeaders DeregisterElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeregisterElasticIp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterElasticIP where
        toJSON DeregisterElasticIP'{..}
          = object
              (catMaybes [Just ("ElasticIp" .= _deipElasticIP)])

instance ToPath DeregisterElasticIP where
        toPath = const "/"

instance ToQuery DeregisterElasticIP where
        toQuery = const mempty

-- | /See:/ 'deregisterElasticIPResponse' smart constructor.
data DeregisterElasticIPResponse =
  DeregisterElasticIPResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterElasticIPResponse' with the minimum fields required to make a request.
--
deregisterElasticIPResponse
    :: DeregisterElasticIPResponse
deregisterElasticIPResponse = DeregisterElasticIPResponse'


instance NFData DeregisterElasticIPResponse where
