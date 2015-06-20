{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
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

-- | Modifies the specified network interface attribute. You can specify only
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyNetworkInterfaceAttribute.html>
module Network.AWS.EC2.ModifyNetworkInterfaceAttribute
    (
    -- * Request
      ModifyNetworkInterfaceAttribute
    -- ** Request constructor
    , modifyNetworkInterfaceAttribute
    -- ** Request lenses
    , mniaGroups
    , mniaSourceDestCheck
    , mniaAttachment
    , mniaDryRun
    , mniaDescription
    , mniaNetworkInterfaceId

    -- * Response
    , ModifyNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , modifyNetworkInterfaceAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyNetworkInterfaceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mniaGroups'
--
-- * 'mniaSourceDestCheck'
--
-- * 'mniaAttachment'
--
-- * 'mniaDryRun'
--
-- * 'mniaDescription'
--
-- * 'mniaNetworkInterfaceId'
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'{_mniaGroups :: Maybe [Text], _mniaSourceDestCheck :: Maybe AttributeBooleanValue, _mniaAttachment :: Maybe NetworkInterfaceAttachmentChanges, _mniaDryRun :: Maybe Bool, _mniaDescription :: Maybe AttributeValue, _mniaNetworkInterfaceId :: Text} deriving (Eq, Read, Show)

-- | 'ModifyNetworkInterfaceAttribute' smart constructor.
modifyNetworkInterfaceAttribute :: Text -> ModifyNetworkInterfaceAttribute
modifyNetworkInterfaceAttribute pNetworkInterfaceId = ModifyNetworkInterfaceAttribute'{_mniaGroups = Nothing, _mniaSourceDestCheck = Nothing, _mniaAttachment = Nothing, _mniaDryRun = Nothing, _mniaDescription = Nothing, _mniaNetworkInterfaceId = pNetworkInterfaceId};

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it\'s just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
mniaGroups :: Lens' ModifyNetworkInterfaceAttribute [Text]
mniaGroups = lens _mniaGroups (\ s a -> s{_mniaGroups = a}) . _Default;

-- | Indicates whether source\/destination checking is enabled. A value of
-- @true@ means checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon Virtual Private Cloud User Guide/.
mniaSourceDestCheck :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeBooleanValue)
mniaSourceDestCheck = lens _mniaSourceDestCheck (\ s a -> s{_mniaSourceDestCheck = a});

-- | Information about the interface attachment. If modifying the \'delete on
-- termination\' attribute, you must specify the ID of the interface
-- attachment.
mniaAttachment :: Lens' ModifyNetworkInterfaceAttribute (Maybe NetworkInterfaceAttachmentChanges)
mniaAttachment = lens _mniaAttachment (\ s a -> s{_mniaAttachment = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mniaDryRun :: Lens' ModifyNetworkInterfaceAttribute (Maybe Bool)
mniaDryRun = lens _mniaDryRun (\ s a -> s{_mniaDryRun = a});

-- | A description for the network interface.
mniaDescription :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeValue)
mniaDescription = lens _mniaDescription (\ s a -> s{_mniaDescription = a});

-- | The ID of the network interface.
mniaNetworkInterfaceId :: Lens' ModifyNetworkInterfaceAttribute Text
mniaNetworkInterfaceId = lens _mniaNetworkInterfaceId (\ s a -> s{_mniaNetworkInterfaceId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ModifyNetworkInterfaceAttribute
         where
        type Sv ModifyNetworkInterfaceAttribute = EC2
        type Rs ModifyNetworkInterfaceAttribute =
             ModifyNetworkInterfaceAttributeResponse
        request = post
        response
          = receiveNull
              ModifyNetworkInterfaceAttributeResponse'

instance ToHeaders ModifyNetworkInterfaceAttribute
         where
        toHeaders = const mempty

instance ToPath ModifyNetworkInterfaceAttribute where
        toPath = const "/"

instance ToQuery ModifyNetworkInterfaceAttribute
         where
        toQuery ModifyNetworkInterfaceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ModifyNetworkInterfaceAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "SecurityGroupId" <$> _mniaGroups),
               "SourceDestCheck" =: _mniaSourceDestCheck,
               "Attachment" =: _mniaAttachment,
               "DryRun" =: _mniaDryRun,
               "Description" =: _mniaDescription,
               "NetworkInterfaceId" =: _mniaNetworkInterfaceId]

-- | /See:/ 'modifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse' deriving (Eq, Read, Show)

-- | 'ModifyNetworkInterfaceAttributeResponse' smart constructor.
modifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse
modifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse';
