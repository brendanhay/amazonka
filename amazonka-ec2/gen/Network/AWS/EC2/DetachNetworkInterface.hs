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
-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a network interface from an instance.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html AWS API Reference> for DetachNetworkInterface.
module Network.AWS.EC2.DetachNetworkInterface
    (
    -- * Creating a Request
      detachNetworkInterface
    , DetachNetworkInterface
    -- * Request Lenses
    , dniForce
    , dniDryRun
    , dniAttachmentId

    -- * Destructuring the Response
    , detachNetworkInterfaceResponse
    , DetachNetworkInterfaceResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachNetworkInterface' smart constructor.
data DetachNetworkInterface = DetachNetworkInterface'
    { _dniForce        :: !(Maybe Bool)
    , _dniDryRun       :: !(Maybe Bool)
    , _dniAttachmentId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachNetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniForce'
--
-- * 'dniDryRun'
--
-- * 'dniAttachmentId'
detachNetworkInterface
    :: Text -- ^ 'dniAttachmentId'
    -> DetachNetworkInterface
detachNetworkInterface pAttachmentId_ =
    DetachNetworkInterface'
    { _dniForce = Nothing
    , _dniDryRun = Nothing
    , _dniAttachmentId = pAttachmentId_
    }

-- | Specifies whether to force a detachment.
dniForce :: Lens' DetachNetworkInterface (Maybe Bool)
dniForce = lens _dniForce (\ s a -> s{_dniForce = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dniDryRun :: Lens' DetachNetworkInterface (Maybe Bool)
dniDryRun = lens _dniDryRun (\ s a -> s{_dniDryRun = a});

-- | The ID of the attachment.
dniAttachmentId :: Lens' DetachNetworkInterface Text
dniAttachmentId = lens _dniAttachmentId (\ s a -> s{_dniAttachmentId = a});

instance AWSRequest DetachNetworkInterface where
        type Rs DetachNetworkInterface =
             DetachNetworkInterfaceResponse
        request = postQuery eC2
        response
          = receiveNull DetachNetworkInterfaceResponse'

instance ToHeaders DetachNetworkInterface where
        toHeaders = const mempty

instance ToPath DetachNetworkInterface where
        toPath = const "/"

instance ToQuery DetachNetworkInterface where
        toQuery DetachNetworkInterface'{..}
          = mconcat
              ["Action" =:
                 ("DetachNetworkInterface" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "Force" =: _dniForce, "DryRun" =: _dniDryRun,
               "AttachmentId" =: _dniAttachmentId]

-- | /See:/ 'detachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse =
    DetachNetworkInterfaceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachNetworkInterfaceResponse' with the minimum fields required to make a request.
--
detachNetworkInterfaceResponse
    :: DetachNetworkInterfaceResponse
detachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
