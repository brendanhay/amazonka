{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DisassociateElasticIP
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from its instance. The address
-- remains registered with the stack. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DisassociateElasticIP.html>
module Network.AWS.OpsWorks.DisassociateElasticIP
    (
    -- * Request
      DisassociateElasticIP
    -- ** Request constructor
    , disassociateElasticIP
    -- ** Request lenses
    , deiElasticIP

    -- * Response
    , DisassociateElasticIPResponse
    -- ** Response constructor
    , disassociateElasticIPResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'disassociateElasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deiElasticIP'
newtype DisassociateElasticIP = DisassociateElasticIP'
    { _deiElasticIP :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateElasticIP' smart constructor.
disassociateElasticIP :: Text -> DisassociateElasticIP
disassociateElasticIP pElasticIP =
    DisassociateElasticIP'
    { _deiElasticIP = pElasticIP
    }

-- | The Elastic IP address.
deiElasticIP :: Lens' DisassociateElasticIP Text
deiElasticIP = lens _deiElasticIP (\ s a -> s{_deiElasticIP = a});

instance AWSRequest DisassociateElasticIP where
        type Sv DisassociateElasticIP = OpsWorks
        type Rs DisassociateElasticIP =
             DisassociateElasticIPResponse
        request = postJSON
        response = receiveNull DisassociateElasticIPResponse'

instance ToHeaders DisassociateElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DisassociateElasticIP" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateElasticIP where
        toJSON DisassociateElasticIP'{..}
          = object ["ElasticIp" .= _deiElasticIP]

instance ToPath DisassociateElasticIP where
        toPath = const "/"

instance ToQuery DisassociateElasticIP where
        toQuery = const mempty

-- | /See:/ 'disassociateElasticIPResponse' smart constructor.
data DisassociateElasticIPResponse =
    DisassociateElasticIPResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisassociateElasticIPResponse' smart constructor.
disassociateElasticIPResponse :: DisassociateElasticIPResponse
disassociateElasticIPResponse = DisassociateElasticIPResponse'
