{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCAttribute
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified VPC. You can specify
-- only one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCAttribute.html>
module Network.AWS.EC2.DescribeVPCAttribute
    (
    -- * Request
      DescribeVPCAttribute
    -- ** Request constructor
    , describeVPCAttribute
    -- ** Request lenses
    , dvpcaAttribute
    , dvpcaDryRun
    , dvpcaVPCId

    -- * Response
    , DescribeVPCAttributeResponse
    -- ** Response constructor
    , describeVPCAttributeResponse
    -- ** Response lenses
    , dvpcarEnableDNSHostnames
    , dvpcarEnableDNSSupport
    , dvpcarVPCId
    , dvpcarStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCAttribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcaAttribute'
--
-- * 'dvpcaDryRun'
--
-- * 'dvpcaVPCId'
data DescribeVPCAttribute = DescribeVPCAttribute'
    { _dvpcaAttribute :: !(Maybe VPCAttributeName)
    , _dvpcaDryRun    :: !(Maybe Bool)
    , _dvpcaVPCId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCAttribute' smart constructor.
describeVPCAttribute :: Text -> DescribeVPCAttribute
describeVPCAttribute pVPCId =
    DescribeVPCAttribute'
    { _dvpcaAttribute = Nothing
    , _dvpcaDryRun = Nothing
    , _dvpcaVPCId = pVPCId
    }

-- | The VPC attribute.
dvpcaAttribute :: Lens' DescribeVPCAttribute (Maybe VPCAttributeName)
dvpcaAttribute = lens _dvpcaAttribute (\ s a -> s{_dvpcaAttribute = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpcaDryRun :: Lens' DescribeVPCAttribute (Maybe Bool)
dvpcaDryRun = lens _dvpcaDryRun (\ s a -> s{_dvpcaDryRun = a});

-- | The ID of the VPC.
dvpcaVPCId :: Lens' DescribeVPCAttribute Text
dvpcaVPCId = lens _dvpcaVPCId (\ s a -> s{_dvpcaVPCId = a});

instance AWSRequest DescribeVPCAttribute where
        type Sv DescribeVPCAttribute = EC2
        type Rs DescribeVPCAttribute =
             DescribeVPCAttributeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCAttributeResponse' <$>
                   (x .@? "enableDnsHostnames") <*>
                     (x .@? "enableDnsSupport")
                     <*> (x .@? "vpcId")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVPCAttribute where
        toHeaders = const mempty

instance ToPath DescribeVPCAttribute where
        toPath = const "/"

instance ToQuery DescribeVPCAttribute where
        toQuery DescribeVPCAttribute'{..}
          = mconcat
              ["Action" =: ("DescribeVPCAttribute" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Attribute" =: _dvpcaAttribute,
               "DryRun" =: _dvpcaDryRun, "VpcId" =: _dvpcaVPCId]

-- | /See:/ 'describeVPCAttributeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcarEnableDNSHostnames'
--
-- * 'dvpcarEnableDNSSupport'
--
-- * 'dvpcarVPCId'
--
-- * 'dvpcarStatus'
data DescribeVPCAttributeResponse = DescribeVPCAttributeResponse'
    { _dvpcarEnableDNSHostnames :: !(Maybe AttributeBooleanValue)
    , _dvpcarEnableDNSSupport   :: !(Maybe AttributeBooleanValue)
    , _dvpcarVPCId              :: !(Maybe Text)
    , _dvpcarStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCAttributeResponse' smart constructor.
describeVPCAttributeResponse :: Int -> DescribeVPCAttributeResponse
describeVPCAttributeResponse pStatus =
    DescribeVPCAttributeResponse'
    { _dvpcarEnableDNSHostnames = Nothing
    , _dvpcarEnableDNSSupport = Nothing
    , _dvpcarVPCId = Nothing
    , _dvpcarStatus = pStatus
    }

-- | Indicates whether the instances launched in the VPC get DNS hostnames.
-- If this attribute is @true@, instances in the VPC get DNS hostnames;
-- otherwise, they do not.
dvpcarEnableDNSHostnames :: Lens' DescribeVPCAttributeResponse (Maybe AttributeBooleanValue)
dvpcarEnableDNSHostnames = lens _dvpcarEnableDNSHostnames (\ s a -> s{_dvpcarEnableDNSHostnames = a});

-- | Indicates whether DNS resolution is enabled for the VPC. If this
-- attribute is @true@, the Amazon DNS server resolves DNS hostnames for
-- your instances to their corresponding IP addresses; otherwise, it does
-- not.
dvpcarEnableDNSSupport :: Lens' DescribeVPCAttributeResponse (Maybe AttributeBooleanValue)
dvpcarEnableDNSSupport = lens _dvpcarEnableDNSSupport (\ s a -> s{_dvpcarEnableDNSSupport = a});

-- | The ID of the VPC.
dvpcarVPCId :: Lens' DescribeVPCAttributeResponse (Maybe Text)
dvpcarVPCId = lens _dvpcarVPCId (\ s a -> s{_dvpcarVPCId = a});

-- | FIXME: Undocumented member.
dvpcarStatus :: Lens' DescribeVPCAttributeResponse Int
dvpcarStatus = lens _dvpcarStatus (\ s a -> s{_dvpcarStatus = a});
