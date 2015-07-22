{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachClassicLinkVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the
-- instance has been unlinked, the VPC security groups are no longer
-- associated with it. An instance is automatically unlinked from a VPC
-- when it\'s stopped.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachClassicLinkVPC.html>
module Network.AWS.EC2.DetachClassicLinkVPC
    (
    -- * Request
      DetachClassicLinkVPC
    -- ** Request constructor
    , detachClassicLinkVPC
    -- ** Request lenses
    , dclvrqDryRun
    , dclvrqInstanceId
    , dclvrqVPCId

    -- * Response
    , DetachClassicLinkVPCResponse
    -- ** Response constructor
    , detachClassicLinkVPCResponse
    -- ** Response lenses
    , dclvrsReturn
    , dclvrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachClassicLinkVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dclvrqDryRun'
--
-- * 'dclvrqInstanceId'
--
-- * 'dclvrqVPCId'
data DetachClassicLinkVPC = DetachClassicLinkVPC'
    { _dclvrqDryRun     :: !(Maybe Bool)
    , _dclvrqInstanceId :: !Text
    , _dclvrqVPCId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachClassicLinkVPC' smart constructor.
detachClassicLinkVPC :: Text -> Text -> DetachClassicLinkVPC
detachClassicLinkVPC pInstanceId pVPCId =
    DetachClassicLinkVPC'
    { _dclvrqDryRun = Nothing
    , _dclvrqInstanceId = pInstanceId
    , _dclvrqVPCId = pVPCId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dclvrqDryRun :: Lens' DetachClassicLinkVPC (Maybe Bool)
dclvrqDryRun = lens _dclvrqDryRun (\ s a -> s{_dclvrqDryRun = a});

-- | The ID of the instance to unlink from the VPC.
dclvrqInstanceId :: Lens' DetachClassicLinkVPC Text
dclvrqInstanceId = lens _dclvrqInstanceId (\ s a -> s{_dclvrqInstanceId = a});

-- | The ID of the VPC to which the instance is linked.
dclvrqVPCId :: Lens' DetachClassicLinkVPC Text
dclvrqVPCId = lens _dclvrqVPCId (\ s a -> s{_dclvrqVPCId = a});

instance AWSRequest DetachClassicLinkVPC where
        type Sv DetachClassicLinkVPC = EC2
        type Rs DetachClassicLinkVPC =
             DetachClassicLinkVPCResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DetachClassicLinkVPCResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance ToHeaders DetachClassicLinkVPC where
        toHeaders = const mempty

instance ToPath DetachClassicLinkVPC where
        toPath = const "/"

instance ToQuery DetachClassicLinkVPC where
        toQuery DetachClassicLinkVPC'{..}
          = mconcat
              ["Action" =: ("DetachClassicLinkVPC" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dclvrqDryRun,
               "InstanceId" =: _dclvrqInstanceId,
               "VpcId" =: _dclvrqVPCId]

-- | /See:/ 'detachClassicLinkVPCResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dclvrsReturn'
--
-- * 'dclvrsStatus'
data DetachClassicLinkVPCResponse = DetachClassicLinkVPCResponse'
    { _dclvrsReturn :: !(Maybe Bool)
    , _dclvrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachClassicLinkVPCResponse' smart constructor.
detachClassicLinkVPCResponse :: Int -> DetachClassicLinkVPCResponse
detachClassicLinkVPCResponse pStatus =
    DetachClassicLinkVPCResponse'
    { _dclvrsReturn = Nothing
    , _dclvrsStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dclvrsReturn :: Lens' DetachClassicLinkVPCResponse (Maybe Bool)
dclvrsReturn = lens _dclvrsReturn (\ s a -> s{_dclvrsReturn = a});

-- | FIXME: Undocumented member.
dclvrsStatus :: Lens' DetachClassicLinkVPCResponse Int
dclvrsStatus = lens _dclvrsStatus (\ s a -> s{_dclvrsStatus = a});
