{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , dniarqAttribute
    , dniarqDryRun
    , dniarqNetworkInterfaceId

    -- * Response
    , DescribeNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , describeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniarsGroups
    , dniarsSourceDestCheck
    , dniarsNetworkInterfaceId
    , dniarsAttachment
    , dniarsDescription
    , dniarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeNetworkInterfaceAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniarqAttribute'
--
-- * 'dniarqDryRun'
--
-- * 'dniarqNetworkInterfaceId'
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
    { _dniarqAttribute          :: !(Maybe NetworkInterfaceAttribute)
    , _dniarqDryRun             :: !(Maybe Bool)
    , _dniarqNetworkInterfaceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeNetworkInterfaceAttribute' smart constructor.
describeNetworkInterfaceAttribute :: Text -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute pNetworkInterfaceId =
    DescribeNetworkInterfaceAttribute'
    { _dniarqAttribute = Nothing
    , _dniarqDryRun = Nothing
    , _dniarqNetworkInterfaceId = pNetworkInterfaceId
    }

-- | The attribute of the network interface.
dniarqAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniarqAttribute = lens _dniarqAttribute (\ s a -> s{_dniarqAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dniarqDryRun :: Lens' DescribeNetworkInterfaceAttribute (Maybe Bool)
dniarqDryRun = lens _dniarqDryRun (\ s a -> s{_dniarqDryRun = a});

-- | The ID of the network interface.
dniarqNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dniarqNetworkInterfaceId = lens _dniarqNetworkInterfaceId (\ s a -> s{_dniarqNetworkInterfaceId = a});

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
               "Attribute" =: _dniarqAttribute,
               "DryRun" =: _dniarqDryRun,
               "NetworkInterfaceId" =: _dniarqNetworkInterfaceId]

-- | /See:/ 'describeNetworkInterfaceAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
    { _dniarsGroups             :: !(Maybe [GroupIdentifier])
    , _dniarsSourceDestCheck    :: !(Maybe AttributeBooleanValue)
    , _dniarsNetworkInterfaceId :: !(Maybe Text)
    , _dniarsAttachment         :: !(Maybe NetworkInterfaceAttachment)
    , _dniarsDescription        :: !(Maybe AttributeValue)
    , _dniarsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeNetworkInterfaceAttributeResponse' smart constructor.
describeNetworkInterfaceAttributeResponse :: Int -> DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttributeResponse pStatus =
    DescribeNetworkInterfaceAttributeResponse'
    { _dniarsGroups = Nothing
    , _dniarsSourceDestCheck = Nothing
    , _dniarsNetworkInterfaceId = Nothing
    , _dniarsAttachment = Nothing
    , _dniarsDescription = Nothing
    , _dniarsStatus = pStatus
    }

-- | The security groups associated with the network interface.
dniarsGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dniarsGroups = lens _dniarsGroups (\ s a -> s{_dniarsGroups = a}) . _Default;

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

-- | FIXME: Undocumented member.
dniarsStatus :: Lens' DescribeNetworkInterfaceAttributeResponse Int
dniarsStatus = lens _dniarsStatus (\ s a -> s{_dniarsStatus = a});
