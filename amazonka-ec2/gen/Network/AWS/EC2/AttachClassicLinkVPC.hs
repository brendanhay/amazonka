{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.AttachClassicLinkVPC
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Links an EC2-Classic instance to a ClassicLink-enabled VPC through one
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
    , aclvDryRun
    , aclvInstanceId
    , aclvVPCId
    , aclvGroups

    -- * Response
    , AttachClassicLinkVPCResponse
    -- ** Response constructor
    , attachClassicLinkVPCResponse
    -- ** Response lenses
    , aclvrReturn
    , aclvrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachClassicLinkVPC' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aclvDryRun'
--
-- * 'aclvInstanceId'
--
-- * 'aclvVPCId'
--
-- * 'aclvGroups'
data AttachClassicLinkVPC = AttachClassicLinkVPC'
    { _aclvDryRun     :: !(Maybe Bool)
    , _aclvInstanceId :: !Text
    , _aclvVPCId      :: !Text
    , _aclvGroups     :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'AttachClassicLinkVPC' smart constructor.
attachClassicLinkVPC :: Text -> Text -> AttachClassicLinkVPC
attachClassicLinkVPC pInstanceId pVPCId =
    AttachClassicLinkVPC'
    { _aclvDryRun = Nothing
    , _aclvInstanceId = pInstanceId
    , _aclvVPCId = pVPCId
    , _aclvGroups = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
aclvDryRun :: Lens' AttachClassicLinkVPC (Maybe Bool)
aclvDryRun = lens _aclvDryRun (\ s a -> s{_aclvDryRun = a});

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled
-- VPC.
aclvInstanceId :: Lens' AttachClassicLinkVPC Text
aclvInstanceId = lens _aclvInstanceId (\ s a -> s{_aclvInstanceId = a});

-- | The ID of a ClassicLink-enabled VPC.
aclvVPCId :: Lens' AttachClassicLinkVPC Text
aclvVPCId = lens _aclvVPCId (\ s a -> s{_aclvVPCId = a});

-- | The ID of one or more of the VPC\'s security groups. You cannot specify
-- security groups from a different VPC.
aclvGroups :: Lens' AttachClassicLinkVPC [Text]
aclvGroups = lens _aclvGroups (\ s a -> s{_aclvGroups = a});

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
               "DryRun" =: _aclvDryRun,
               "InstanceId" =: _aclvInstanceId,
               "VpcId" =: _aclvVPCId,
               toQueryList "groupId" _aclvGroups]

-- | /See:/ 'attachClassicLinkVPCResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aclvrReturn'
--
-- * 'aclvrStatus'
data AttachClassicLinkVPCResponse = AttachClassicLinkVPCResponse'
    { _aclvrReturn :: !(Maybe Bool)
    , _aclvrStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'AttachClassicLinkVPCResponse' smart constructor.
attachClassicLinkVPCResponse :: Int -> AttachClassicLinkVPCResponse
attachClassicLinkVPCResponse pStatus =
    AttachClassicLinkVPCResponse'
    { _aclvrReturn = Nothing
    , _aclvrStatus = pStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
aclvrReturn :: Lens' AttachClassicLinkVPCResponse (Maybe Bool)
aclvrReturn = lens _aclvrReturn (\ s a -> s{_aclvrReturn = a});

-- | FIXME: Undocumented member.
aclvrStatus :: Lens' AttachClassicLinkVPCResponse Int
aclvrStatus = lens _aclvrStatus (\ s a -> s{_aclvrStatus = a});
