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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one
-- attribute at a time.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaceAttribute.html AWS API Reference> for DescribeNetworkInterfaceAttribute.
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
    , dniarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
    { _dniaAttribute          :: !(Maybe NetworkInterfaceAttribute)
    , _dniaDryRun             :: !(Maybe Bool)
    , _dniaNetworkInterfaceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniaAttribute'
--
-- * 'dniaDryRun'
--
-- * 'dniaNetworkInterfaceId'
describeNetworkInterfaceAttribute
    :: Text -- ^ 'dniaNetworkInterfaceId'
    -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute pNetworkInterfaceId_ =
    DescribeNetworkInterfaceAttribute'
    { _dniaAttribute = Nothing
    , _dniaDryRun = Nothing
    , _dniaNetworkInterfaceId = pNetworkInterfaceId_
    }

-- | The attribute of the network interface.
dniaAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniaAttribute = lens _dniaAttribute (\ s a -> s{_dniaAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dniaDryRun :: Lens' DescribeNetworkInterfaceAttribute (Maybe Bool)
dniaDryRun = lens _dniaDryRun (\ s a -> s{_dniaDryRun = a});

-- | The ID of the network interface.
dniaNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dniaNetworkInterfaceId = lens _dniaNetworkInterfaceId (\ s a -> s{_dniaNetworkInterfaceId = a});

instance AWSRequest DescribeNetworkInterfaceAttribute
         where
        type Rs DescribeNetworkInterfaceAttribute =
             DescribeNetworkInterfaceAttributeResponse
        request = postQuery eC2
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
               "Version" =: ("2015-04-15" :: ByteString),
               "Attribute" =: _dniaAttribute,
               "DryRun" =: _dniaDryRun,
               "NetworkInterfaceId" =: _dniaNetworkInterfaceId]

-- | /See:/ 'describeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
    { _dniarsGroups             :: !(Maybe [GroupIdentifier])
    , _dniarsSourceDestCheck    :: !(Maybe AttributeBooleanValue)
    , _dniarsNetworkInterfaceId :: !(Maybe Text)
    , _dniarsAttachment         :: !(Maybe NetworkInterfaceAttachment)
    , _dniarsDescription        :: !(Maybe AttributeValue)
    , _dniarsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniarsGroups'
--
-- * 'dniarsSourceDestCheck'
--
-- * 'dniarsNetworkInterfaceId'
--
-- * 'dniarsAttachment'
--
-- * 'dniarsDescription'
--
-- * 'dniarsStatus'
describeNetworkInterfaceAttributeResponse
    :: Int -- ^ 'dniarsStatus'
    -> DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttributeResponse pStatus_ =
    DescribeNetworkInterfaceAttributeResponse'
    { _dniarsGroups = Nothing
    , _dniarsSourceDestCheck = Nothing
    , _dniarsNetworkInterfaceId = Nothing
    , _dniarsAttachment = Nothing
    , _dniarsDescription = Nothing
    , _dniarsStatus = pStatus_
    }

-- | The security groups associated with the network interface.
dniarsGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dniarsGroups = lens _dniarsGroups (\ s a -> s{_dniarsGroups = a}) . _Default . _Coerce;

-- | Indicates whether source\/destination checking is enabled.
dniarsSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dniarsSourceDestCheck = lens _dniarsSourceDestCheck (\ s a -> s{_dniarsSourceDestCheck = a});

-- | The ID of the network interface.
dniarsNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dniarsNetworkInterfaceId = lens _dniarsNetworkInterfaceId (\ s a -> s{_dniarsNetworkInterfaceId = a});

-- | The attachment (if any) of the network interface.
dniarsAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dniarsAttachment = lens _dniarsAttachment (\ s a -> s{_dniarsAttachment = a});

-- | The description of the network interface.
dniarsDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dniarsDescription = lens _dniarsDescription (\ s a -> s{_dniarsDescription = a});

-- | The response status code.
dniarsStatus :: Lens' DescribeNetworkInterfaceAttributeResponse Int
dniarsStatus = lens _dniarsStatus (\ s a -> s{_dniarsStatus = a});
