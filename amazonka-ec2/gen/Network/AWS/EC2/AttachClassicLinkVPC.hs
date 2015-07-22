{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachClassicLinkVPC
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Links an EC2-Classic instance to a ClassicLink-enabled VPC through one
-- or more of the VPC\'s security groups. You cannot link an EC2-Classic
-- instance to more than one VPC at a time. You can only link an instance
-- that\'s in the @running@ state. An instance is automatically unlinked
-- from a VPC when it\'s stopped - you can link it to the VPC again when
-- you restart it.
--
-- After you\'ve linked an instance, you cannot change the VPC security
-- groups that are associated with it. To change the security groups, you
-- must first unlink the instance, and then link it again.
--
-- Linking your instance to a VPC is sometimes referred to as /attaching/
-- your instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachClassicLinkVPC.html>
module Network.AWS.EC2.AttachClassicLinkVPC
    (
    -- * Request
      AttachClassicLinkVPC
    -- ** Request constructor
    , attachClassicLinkVPC
    -- ** Request lenses
    , aclvrqDryRun
    , aclvrqInstanceId
    , aclvrqVPCId
    , aclvrqGroups

    -- * Response
    , AttachClassicLinkVPCResponse
    -- ** Response constructor
    , attachClassicLinkVPCResponse
    -- ** Response lenses
    , aclvrsReturn
    , aclvrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachClassicLinkVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aclvrqDryRun'
--
-- * 'aclvrqInstanceId'
--
-- * 'aclvrqVPCId'
--
-- * 'aclvrqGroups'
data AttachClassicLinkVPC = AttachClassicLinkVPC'
    { _aclvrqDryRun     :: !(Maybe Bool)
    , _aclvrqInstanceId :: !Text
    , _aclvrqVPCId      :: !Text
    , _aclvrqGroups     :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachClassicLinkVPC' smart constructor.
attachClassicLinkVPC :: Text -> Text -> AttachClassicLinkVPC
attachClassicLinkVPC pInstanceId pVPCId =
    AttachClassicLinkVPC'
    { _aclvrqDryRun = Nothing
    , _aclvrqInstanceId = pInstanceId
    , _aclvrqVPCId = pVPCId
    , _aclvrqGroups = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
aclvrqDryRun :: Lens' AttachClassicLinkVPC (Maybe Bool)
aclvrqDryRun = lens _aclvrqDryRun (\ s a -> s{_aclvrqDryRun = a});

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled
-- VPC.
aclvrqInstanceId :: Lens' AttachClassicLinkVPC Text
aclvrqInstanceId = lens _aclvrqInstanceId (\ s a -> s{_aclvrqInstanceId = a});

-- | The ID of a ClassicLink-enabled VPC.
aclvrqVPCId :: Lens' AttachClassicLinkVPC Text
aclvrqVPCId = lens _aclvrqVPCId (\ s a -> s{_aclvrqVPCId = a});

-- | The ID of one or more of the VPC\'s security groups. You cannot specify
-- security groups from a different VPC.
aclvrqGroups :: Lens' AttachClassicLinkVPC [Text]
aclvrqGroups = lens _aclvrqGroups (\ s a -> s{_aclvrqGroups = a});

instance AWSRequest AttachClassicLinkVPC where
        type Sv AttachClassicLinkVPC = EC2
        type Rs AttachClassicLinkVPC =
             AttachClassicLinkVPCResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 AttachClassicLinkVPCResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance ToHeaders AttachClassicLinkVPC where
        toHeaders = const mempty

instance ToPath AttachClassicLinkVPC where
        toPath = const "/"

instance ToQuery AttachClassicLinkVPC where
        toQuery AttachClassicLinkVPC'{..}
          = mconcat
              ["Action" =: ("AttachClassicLinkVPC" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _aclvrqDryRun,
               "InstanceId" =: _aclvrqInstanceId,
               "VpcId" =: _aclvrqVPCId,
               toQueryList "groupId" _aclvrqGroups]

-- | /See:/ 'attachClassicLinkVPCResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aclvrsReturn'
--
-- * 'aclvrsStatus'
data AttachClassicLinkVPCResponse = AttachClassicLinkVPCResponse'
    { _aclvrsReturn :: !(Maybe Bool)
    , _aclvrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachClassicLinkVPCResponse' smart constructor.
attachClassicLinkVPCResponse :: Int -> AttachClassicLinkVPCResponse
attachClassicLinkVPCResponse pStatus =
    AttachClassicLinkVPCResponse'
    { _aclvrsReturn = Nothing
    , _aclvrsStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
aclvrsReturn :: Lens' AttachClassicLinkVPCResponse (Maybe Bool)
aclvrsReturn = lens _aclvrsReturn (\ s a -> s{_aclvrsReturn = a});

-- | FIXME: Undocumented member.
aclvrsStatus :: Lens' AttachClassicLinkVPCResponse Int
aclvrsStatus = lens _aclvrsStatus (\ s a -> s{_aclvrsStatus = a});
