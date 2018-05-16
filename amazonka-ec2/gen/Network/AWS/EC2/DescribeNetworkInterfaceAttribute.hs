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
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one attribute at a time.
--
--
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
    (
    -- * Creating a Request
      describeNetworkInterfaceAttribute
    , DescribeNetworkInterfaceAttribute
    -- * Request Lenses
    , dniaAttribute
    , dniaDryRun
    , dniaNetworkInterfaceId

    -- * Destructuring the Response
    , describeNetworkInterfaceAttributeResponse
    , DescribeNetworkInterfaceAttributeResponse
    -- * Response Lenses
    , dniarsGroups
    , dniarsSourceDestCheck
    , dniarsNetworkInterfaceId
    , dniarsAttachment
    , dniarsDescription
    , dniarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeNetworkInterfaceAttribute.
--
--
--
-- /See:/ 'describeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
  { _dniaAttribute          :: !(Maybe NetworkInterfaceAttribute)
  , _dniaDryRun             :: !(Maybe Bool)
  , _dniaNetworkInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniaAttribute' - The attribute of the network interface. This parameter is required.
--
-- * 'dniaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dniaNetworkInterfaceId' - The ID of the network interface.
describeNetworkInterfaceAttribute
    :: Text -- ^ 'dniaNetworkInterfaceId'
    -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute pNetworkInterfaceId_ =
  DescribeNetworkInterfaceAttribute'
    { _dniaAttribute = Nothing
    , _dniaDryRun = Nothing
    , _dniaNetworkInterfaceId = pNetworkInterfaceId_
    }


-- | The attribute of the network interface. This parameter is required.
dniaAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniaAttribute = lens _dniaAttribute (\ s a -> s{_dniaAttribute = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dniaDryRun :: Lens' DescribeNetworkInterfaceAttribute (Maybe Bool)
dniaDryRun = lens _dniaDryRun (\ s a -> s{_dniaDryRun = a})

-- | The ID of the network interface.
dniaNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dniaNetworkInterfaceId = lens _dniaNetworkInterfaceId (\ s a -> s{_dniaNetworkInterfaceId = a})

instance AWSRequest DescribeNetworkInterfaceAttribute
         where
        type Rs DescribeNetworkInterfaceAttribute =
             DescribeNetworkInterfaceAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeNetworkInterfaceAttributeResponse' <$>
                   (x .@? "groupSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "sourceDestCheck")
                     <*> (x .@? "networkInterfaceId")
                     <*> (x .@? "attachment")
                     <*> (x .@? "description")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNetworkInterfaceAttribute
         where

instance NFData DescribeNetworkInterfaceAttribute
         where

instance ToHeaders DescribeNetworkInterfaceAttribute
         where
        toHeaders = const mempty

instance ToPath DescribeNetworkInterfaceAttribute
         where
        toPath = const "/"

instance ToQuery DescribeNetworkInterfaceAttribute
         where
        toQuery DescribeNetworkInterfaceAttribute'{..}
          = mconcat
              ["Action" =:
                 ("DescribeNetworkInterfaceAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Attribute" =: _dniaAttribute,
               "DryRun" =: _dniaDryRun,
               "NetworkInterfaceId" =: _dniaNetworkInterfaceId]

-- | Contains the output of DescribeNetworkInterfaceAttribute.
--
--
--
-- /See:/ 'describeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
  { _dniarsGroups             :: !(Maybe [GroupIdentifier])
  , _dniarsSourceDestCheck    :: !(Maybe AttributeBooleanValue)
  , _dniarsNetworkInterfaceId :: !(Maybe Text)
  , _dniarsAttachment         :: !(Maybe NetworkInterfaceAttachment)
  , _dniarsDescription        :: !(Maybe AttributeValue)
  , _dniarsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniarsGroups' - The security groups associated with the network interface.
--
-- * 'dniarsSourceDestCheck' - Indicates whether source/destination checking is enabled.
--
-- * 'dniarsNetworkInterfaceId' - The ID of the network interface.
--
-- * 'dniarsAttachment' - The attachment (if any) of the network interface.
--
-- * 'dniarsDescription' - The description of the network interface.
--
-- * 'dniarsResponseStatus' - -- | The response status code.
describeNetworkInterfaceAttributeResponse
    :: Int -- ^ 'dniarsResponseStatus'
    -> DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttributeResponse pResponseStatus_ =
  DescribeNetworkInterfaceAttributeResponse'
    { _dniarsGroups = Nothing
    , _dniarsSourceDestCheck = Nothing
    , _dniarsNetworkInterfaceId = Nothing
    , _dniarsAttachment = Nothing
    , _dniarsDescription = Nothing
    , _dniarsResponseStatus = pResponseStatus_
    }


-- | The security groups associated with the network interface.
dniarsGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dniarsGroups = lens _dniarsGroups (\ s a -> s{_dniarsGroups = a}) . _Default . _Coerce

-- | Indicates whether source/destination checking is enabled.
dniarsSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dniarsSourceDestCheck = lens _dniarsSourceDestCheck (\ s a -> s{_dniarsSourceDestCheck = a})

-- | The ID of the network interface.
dniarsNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dniarsNetworkInterfaceId = lens _dniarsNetworkInterfaceId (\ s a -> s{_dniarsNetworkInterfaceId = a})

-- | The attachment (if any) of the network interface.
dniarsAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dniarsAttachment = lens _dniarsAttachment (\ s a -> s{_dniarsAttachment = a})

-- | The description of the network interface.
dniarsDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dniarsDescription = lens _dniarsDescription (\ s a -> s{_dniarsDescription = a})

-- | -- | The response status code.
dniarsResponseStatus :: Lens' DescribeNetworkInterfaceAttributeResponse Int
dniarsResponseStatus = lens _dniarsResponseStatus (\ s a -> s{_dniarsResponseStatus = a})

instance NFData
           DescribeNetworkInterfaceAttributeResponse
         where
