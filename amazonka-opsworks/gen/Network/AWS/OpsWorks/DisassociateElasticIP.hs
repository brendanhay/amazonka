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
-- Module      : Network.AWS.OpsWorks.DisassociateElasticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from its instance. The address remains registered with the stack. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DisassociateElasticIP
    (
    -- * Creating a Request
      disassociateElasticIP
    , DisassociateElasticIP
    -- * Request Lenses
    , deiElasticIP

    -- * Destructuring the Response
    , disassociateElasticIPResponse
    , DisassociateElasticIPResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateElasticIP' smart constructor.
newtype DisassociateElasticIP = DisassociateElasticIP'
  { _deiElasticIP :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateElasticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deiElasticIP' - The Elastic IP address.
disassociateElasticIP
    :: Text -- ^ 'deiElasticIP'
    -> DisassociateElasticIP
disassociateElasticIP pElasticIP_ =
  DisassociateElasticIP' {_deiElasticIP = pElasticIP_}


-- | The Elastic IP address.
deiElasticIP :: Lens' DisassociateElasticIP Text
deiElasticIP = lens _deiElasticIP (\ s a -> s{_deiElasticIP = a})

instance AWSRequest DisassociateElasticIP where
        type Rs DisassociateElasticIP =
             DisassociateElasticIPResponse
        request = postJSON opsWorks
        response = receiveNull DisassociateElasticIPResponse'

instance Hashable DisassociateElasticIP where

instance NFData DisassociateElasticIP where

instance ToHeaders DisassociateElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DisassociateElasticIp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateElasticIP where
        toJSON DisassociateElasticIP'{..}
          = object
              (catMaybes [Just ("ElasticIp" .= _deiElasticIP)])

instance ToPath DisassociateElasticIP where
        toPath = const "/"

instance ToQuery DisassociateElasticIP where
        toQuery = const mempty

-- | /See:/ 'disassociateElasticIPResponse' smart constructor.
data DisassociateElasticIPResponse =
  DisassociateElasticIPResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateElasticIPResponse' with the minimum fields required to make a request.
--
disassociateElasticIPResponse
    :: DisassociateElasticIPResponse
disassociateElasticIPResponse = DisassociateElasticIPResponse'


instance NFData DisassociateElasticIPResponse where
