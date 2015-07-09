{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one
-- attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaceAttribute.html>
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
    (
    -- * Request
      DescribeNetworkInterfaceAttribute
    -- ** Request constructor
    , describeNetworkInterfaceAttribute
    -- ** Request lenses
    , dniaAttribute
    , dniaDryRun
    , dniaNetworkInterfaceId

    -- * Response
    , DescribeNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , describeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniarGroups
    , dniarSourceDestCheck
    , dniarNetworkInterfaceId
    , dniarAttachment
    , dniarDescription
    , dniarStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeNetworkInterfaceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniaAttribute'
--
-- * 'dniaDryRun'
--
-- * 'dniaNetworkInterfaceId'
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
    { _dniaAttribute          :: !(Maybe NetworkInterfaceAttribute)
    , _dniaDryRun             :: !(Maybe Bool)
    , _dniaNetworkInterfaceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeNetworkInterfaceAttribute' smart constructor.
describeNetworkInterfaceAttribute :: Text -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute pNetworkInterfaceId =
    DescribeNetworkInterfaceAttribute'
    { _dniaAttribute = Nothing
    , _dniaDryRun = Nothing
    , _dniaNetworkInterfaceId = pNetworkInterfaceId
    }

-- | The attribute of the network interface.
dniaAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniaAttribute = lens _dniaAttribute (\ s a -> s{_dniaAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dniaDryRun :: Lens' DescribeNetworkInterfaceAttribute (Maybe Bool)
dniaDryRun = lens _dniaDryRun (\ s a -> s{_dniaDryRun = a});

-- | The ID of the network interface.
dniaNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dniaNetworkInterfaceId = lens _dniaNetworkInterfaceId (\ s a -> s{_dniaNetworkInterfaceId = a});

instance AWSRequest DescribeNetworkInterfaceAttribute
         where
        type Sv DescribeNetworkInterfaceAttribute = EC2
        type Rs DescribeNetworkInterfaceAttribute =
             DescribeNetworkInterfaceAttributeResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniarGroups'
--
-- * 'dniarSourceDestCheck'
--
-- * 'dniarNetworkInterfaceId'
--
-- * 'dniarAttachment'
--
-- * 'dniarDescription'
--
-- * 'dniarStatus'
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
    { _dniarGroups             :: !(Maybe [GroupIdentifier])
    , _dniarSourceDestCheck    :: !(Maybe AttributeBooleanValue)
    , _dniarNetworkInterfaceId :: !(Maybe Text)
    , _dniarAttachment         :: !(Maybe NetworkInterfaceAttachment)
    , _dniarDescription        :: !(Maybe AttributeValue)
    , _dniarStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeNetworkInterfaceAttributeResponse' smart constructor.
describeNetworkInterfaceAttributeResponse :: Int -> DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttributeResponse pStatus =
    DescribeNetworkInterfaceAttributeResponse'
    { _dniarGroups = Nothing
    , _dniarSourceDestCheck = Nothing
    , _dniarNetworkInterfaceId = Nothing
    , _dniarAttachment = Nothing
    , _dniarDescription = Nothing
    , _dniarStatus = pStatus
    }

-- | The security groups associated with the network interface.
dniarGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dniarGroups = lens _dniarGroups (\ s a -> s{_dniarGroups = a}) . _Default;

-- | Indicates whether source\/destination checking is enabled.
dniarSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dniarSourceDestCheck = lens _dniarSourceDestCheck (\ s a -> s{_dniarSourceDestCheck = a});

-- | The ID of the network interface.
dniarNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dniarNetworkInterfaceId = lens _dniarNetworkInterfaceId (\ s a -> s{_dniarNetworkInterfaceId = a});

-- | The attachment (if any) of the network interface.
dniarAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dniarAttachment = lens _dniarAttachment (\ s a -> s{_dniarAttachment = a});

-- | The description of the network interface.
dniarDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dniarDescription = lens _dniarDescription (\ s a -> s{_dniarDescription = a});

-- | FIXME: Undocumented member.
dniarStatus :: Lens' DescribeNetworkInterfaceAttributeResponse Int
dniarStatus = lens _dniarStatus (\ s a -> s{_dniarStatus = a});
