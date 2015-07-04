{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Detaches a network interface from an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html>
module Network.AWS.EC2.DetachNetworkInterface
    (
    -- * Request
      DetachNetworkInterface
    -- ** Request constructor
    , detachNetworkInterface
    -- ** Request lenses
    , dniForce
    , dniDryRun
    , dniAttachmentId

    -- * Response
    , DetachNetworkInterfaceResponse
    -- ** Response constructor
    , detachNetworkInterfaceResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachNetworkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniForce'
--
-- * 'dniDryRun'
--
-- * 'dniAttachmentId'
data DetachNetworkInterface = DetachNetworkInterface'
    { _dniForce        :: !(Maybe Bool)
    , _dniDryRun       :: !(Maybe Bool)
    , _dniAttachmentId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachNetworkInterface' smart constructor.
detachNetworkInterface :: Text -> DetachNetworkInterface
detachNetworkInterface pAttachmentId =
    DetachNetworkInterface'
    { _dniForce = Nothing
    , _dniDryRun = Nothing
    , _dniAttachmentId = pAttachmentId
    }

-- | Specifies whether to force a detachment.
dniForce :: Lens' DetachNetworkInterface (Maybe Bool)
dniForce = lens _dniForce (\ s a -> s{_dniForce = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dniDryRun :: Lens' DetachNetworkInterface (Maybe Bool)
dniDryRun = lens _dniDryRun (\ s a -> s{_dniDryRun = a});

-- | The ID of the attachment.
dniAttachmentId :: Lens' DetachNetworkInterface Text
dniAttachmentId = lens _dniAttachmentId (\ s a -> s{_dniAttachmentId = a});

instance AWSRequest DetachNetworkInterface where
        type Sv DetachNetworkInterface = EC2
        type Rs DetachNetworkInterface =
             DetachNetworkInterfaceResponse
        request = post
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
               "Version" =: ("2015-04-15" :: ByteString),
               "Force" =: _dniForce, "DryRun" =: _dniDryRun,
               "AttachmentId" =: _dniAttachmentId]

-- | /See:/ 'detachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse =
    DetachNetworkInterfaceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachNetworkInterfaceResponse' smart constructor.
detachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse
detachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
