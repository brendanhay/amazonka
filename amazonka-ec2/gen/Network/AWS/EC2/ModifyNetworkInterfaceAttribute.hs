{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified network interface attribute. You can specify only
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
    , mniarqGroups
    , mniarqSourceDestCheck
    , mniarqAttachment
    , mniarqDryRun
    , mniarqDescription
    , mniarqNetworkInterfaceId

    -- * Response
    , ModifyNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , modifyNetworkInterfaceAttributeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyNetworkInterfaceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mniarqGroups'
--
-- * 'mniarqSourceDestCheck'
--
-- * 'mniarqAttachment'
--
-- * 'mniarqDryRun'
--
-- * 'mniarqDescription'
--
-- * 'mniarqNetworkInterfaceId'
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
    { _mniarqGroups             :: !(Maybe [Text])
    , _mniarqSourceDestCheck    :: !(Maybe AttributeBooleanValue)
    , _mniarqAttachment         :: !(Maybe NetworkInterfaceAttachmentChanges)
    , _mniarqDryRun             :: !(Maybe Bool)
    , _mniarqDescription        :: !(Maybe AttributeValue)
    , _mniarqNetworkInterfaceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyNetworkInterfaceAttribute' smart constructor.
modifyNetworkInterfaceAttribute :: Text -> ModifyNetworkInterfaceAttribute
modifyNetworkInterfaceAttribute pNetworkInterfaceId_ =
    ModifyNetworkInterfaceAttribute'
    { _mniarqGroups = Nothing
    , _mniarqSourceDestCheck = Nothing
    , _mniarqAttachment = Nothing
    , _mniarqDryRun = Nothing
    , _mniarqDescription = Nothing
    , _mniarqNetworkInterfaceId = pNetworkInterfaceId_
    }

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it\'s just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
mniarqGroups :: Lens' ModifyNetworkInterfaceAttribute [Text]
mniarqGroups = lens _mniarqGroups (\ s a -> s{_mniarqGroups = a}) . _Default;

-- | Indicates whether source\/destination checking is enabled. A value of
-- @true@ means checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon Virtual Private Cloud User Guide/.
mniarqSourceDestCheck :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeBooleanValue)
mniarqSourceDestCheck = lens _mniarqSourceDestCheck (\ s a -> s{_mniarqSourceDestCheck = a});

-- | Information about the interface attachment. If modifying the \'delete on
-- termination\' attribute, you must specify the ID of the interface
-- attachment.
mniarqAttachment :: Lens' ModifyNetworkInterfaceAttribute (Maybe NetworkInterfaceAttachmentChanges)
mniarqAttachment = lens _mniarqAttachment (\ s a -> s{_mniarqAttachment = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
mniarqDryRun :: Lens' ModifyNetworkInterfaceAttribute (Maybe Bool)
mniarqDryRun = lens _mniarqDryRun (\ s a -> s{_mniarqDryRun = a});

-- | A description for the network interface.
mniarqDescription :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeValue)
mniarqDescription = lens _mniarqDescription (\ s a -> s{_mniarqDescription = a});

-- | The ID of the network interface.
mniarqNetworkInterfaceId :: Lens' ModifyNetworkInterfaceAttribute Text
mniarqNetworkInterfaceId = lens _mniarqNetworkInterfaceId (\ s a -> s{_mniarqNetworkInterfaceId = a});

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
                 (toQueryList "SecurityGroupId" <$> _mniarqGroups),
               "SourceDestCheck" =: _mniarqSourceDestCheck,
               "Attachment" =: _mniarqAttachment,
               "DryRun" =: _mniarqDryRun,
               "Description" =: _mniarqDescription,
               "NetworkInterfaceId" =: _mniarqNetworkInterfaceId]

-- | /See:/ 'modifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse =
    ModifyNetworkInterfaceAttributeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyNetworkInterfaceAttributeResponse' smart constructor.
modifyNetworkInterfaceAttributeResponse :: ModifyNetworkInterfaceAttributeResponse
modifyNetworkInterfaceAttributeResponse =
    ModifyNetworkInterfaceAttributeResponse'
