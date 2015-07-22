{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Detaches a network interface from an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html>
module Network.AWS.EC2.DetachNetworkInterface
    (
    -- * Request
      DetachNetworkInterface
    -- ** Request constructor
    , detachNetworkInterface
    -- ** Request lenses
    , dnirqForce
    , dnirqDryRun
    , dnirqAttachmentId

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
-- * 'dnirqForce'
--
-- * 'dnirqDryRun'
--
-- * 'dnirqAttachmentId'
data DetachNetworkInterface = DetachNetworkInterface'
    { _dnirqForce        :: !(Maybe Bool)
    , _dnirqDryRun       :: !(Maybe Bool)
    , _dnirqAttachmentId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachNetworkInterface' smart constructor.
detachNetworkInterface :: Text -> DetachNetworkInterface
detachNetworkInterface pAttachmentId_ =
    DetachNetworkInterface'
    { _dnirqForce = Nothing
    , _dnirqDryRun = Nothing
    , _dnirqAttachmentId = pAttachmentId_
    }

-- | Specifies whether to force a detachment.
dnirqForce :: Lens' DetachNetworkInterface (Maybe Bool)
dnirqForce = lens _dnirqForce (\ s a -> s{_dnirqForce = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dnirqDryRun :: Lens' DetachNetworkInterface (Maybe Bool)
dnirqDryRun = lens _dnirqDryRun (\ s a -> s{_dnirqDryRun = a});

-- | The ID of the attachment.
dnirqAttachmentId :: Lens' DetachNetworkInterface Text
dnirqAttachmentId = lens _dnirqAttachmentId (\ s a -> s{_dnirqAttachmentId = a});

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
               "Force" =: _dnirqForce, "DryRun" =: _dnirqDryRun,
               "AttachmentId" =: _dnirqAttachmentId]

-- | /See:/ 'detachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse =
    DetachNetworkInterfaceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachNetworkInterfaceResponse' smart constructor.
detachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse
detachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
