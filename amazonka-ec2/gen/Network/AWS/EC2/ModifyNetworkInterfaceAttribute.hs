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
-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified network interface attribute. You can specify only one attribute at a time.
--
--
module Network.AWS.EC2.ModifyNetworkInterfaceAttribute
    (
    -- * Creating a Request
      modifyNetworkInterfaceAttribute
    , ModifyNetworkInterfaceAttribute
    -- * Request Lenses
    , mniaGroups
    , mniaSourceDestCheck
    , mniaAttachment
    , mniaDescription
    , mniaDryRun
    , mniaNetworkInterfaceId

    -- * Destructuring the Response
    , modifyNetworkInterfaceAttributeResponse
    , ModifyNetworkInterfaceAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyNetworkInterfaceAttribute.
--
--
--
-- /See:/ 'modifyNetworkInterfaceAttribute' smart constructor.
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
  { _mniaGroups             :: !(Maybe [Text])
  , _mniaSourceDestCheck    :: !(Maybe AttributeBooleanValue)
  , _mniaAttachment         :: !(Maybe NetworkInterfaceAttachmentChanges)
  , _mniaDescription        :: !(Maybe AttributeValue)
  , _mniaDryRun             :: !(Maybe Bool)
  , _mniaNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mniaGroups' - Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
--
-- * 'mniaSourceDestCheck' - Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- * 'mniaAttachment' - Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
--
-- * 'mniaDescription' - A description for the network interface.
--
-- * 'mniaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mniaNetworkInterfaceId' - The ID of the network interface.
modifyNetworkInterfaceAttribute
    :: Text -- ^ 'mniaNetworkInterfaceId'
    -> ModifyNetworkInterfaceAttribute
modifyNetworkInterfaceAttribute pNetworkInterfaceId_ =
  ModifyNetworkInterfaceAttribute'
    { _mniaGroups = Nothing
    , _mniaSourceDestCheck = Nothing
    , _mniaAttachment = Nothing
    , _mniaDescription = Nothing
    , _mniaDryRun = Nothing
    , _mniaNetworkInterfaceId = pNetworkInterfaceId_
    }


-- | Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
mniaGroups :: Lens' ModifyNetworkInterfaceAttribute [Text]
mniaGroups = lens _mniaGroups (\ s a -> s{_mniaGroups = a}) . _Default . _Coerce

-- | Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
mniaSourceDestCheck :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeBooleanValue)
mniaSourceDestCheck = lens _mniaSourceDestCheck (\ s a -> s{_mniaSourceDestCheck = a})

-- | Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
mniaAttachment :: Lens' ModifyNetworkInterfaceAttribute (Maybe NetworkInterfaceAttachmentChanges)
mniaAttachment = lens _mniaAttachment (\ s a -> s{_mniaAttachment = a})

-- | A description for the network interface.
mniaDescription :: Lens' ModifyNetworkInterfaceAttribute (Maybe AttributeValue)
mniaDescription = lens _mniaDescription (\ s a -> s{_mniaDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mniaDryRun :: Lens' ModifyNetworkInterfaceAttribute (Maybe Bool)
mniaDryRun = lens _mniaDryRun (\ s a -> s{_mniaDryRun = a})

-- | The ID of the network interface.
mniaNetworkInterfaceId :: Lens' ModifyNetworkInterfaceAttribute Text
mniaNetworkInterfaceId = lens _mniaNetworkInterfaceId (\ s a -> s{_mniaNetworkInterfaceId = a})

instance AWSRequest ModifyNetworkInterfaceAttribute
         where
        type Rs ModifyNetworkInterfaceAttribute =
             ModifyNetworkInterfaceAttributeResponse
        request = postQuery ec2
        response
          = receiveNull
              ModifyNetworkInterfaceAttributeResponse'

instance Hashable ModifyNetworkInterfaceAttribute
         where

instance NFData ModifyNetworkInterfaceAttribute where

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
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "SecurityGroupId" <$> _mniaGroups),
               "SourceDestCheck" =: _mniaSourceDestCheck,
               "Attachment" =: _mniaAttachment,
               "Description" =: _mniaDescription,
               "DryRun" =: _mniaDryRun,
               "NetworkInterfaceId" =: _mniaNetworkInterfaceId]

-- | /See:/ 'modifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse =
  ModifyNetworkInterfaceAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
--
modifyNetworkInterfaceAttributeResponse
    :: ModifyNetworkInterfaceAttributeResponse
modifyNetworkInterfaceAttributeResponse =
  ModifyNetworkInterfaceAttributeResponse'


instance NFData
           ModifyNetworkInterfaceAttributeResponse
         where
