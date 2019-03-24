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
-- Module      : Network.AWS.EC2.DescribeVPCAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified VPC. You can specify only one attribute at a time.
--
--
module Network.AWS.EC2.DescribeVPCAttribute
    (
    -- * Creating a Request
      describeVPCAttribute
    , DescribeVPCAttribute
    -- * Request Lenses
    , dvpcaDryRun
    , dvpcaAttribute
    , dvpcaVPCId

    -- * Destructuring the Response
    , describeVPCAttributeResponse
    , DescribeVPCAttributeResponse
    -- * Response Lenses
    , dvpcarsEnableDNSHostnames
    , dvpcarsEnableDNSSupport
    , dvpcarsVPCId
    , dvpcarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCAttribute' smart constructor.
data DescribeVPCAttribute = DescribeVPCAttribute'
  { _dvpcaDryRun    :: !(Maybe Bool)
  , _dvpcaAttribute :: !VPCAttributeName
  , _dvpcaVPCId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpcaAttribute' - The VPC attribute.
--
-- * 'dvpcaVPCId' - The ID of the VPC.
describeVPCAttribute
    :: VPCAttributeName -- ^ 'dvpcaAttribute'
    -> Text -- ^ 'dvpcaVPCId'
    -> DescribeVPCAttribute
describeVPCAttribute pAttribute_ pVPCId_ =
  DescribeVPCAttribute'
    { _dvpcaDryRun = Nothing
    , _dvpcaAttribute = pAttribute_
    , _dvpcaVPCId = pVPCId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcaDryRun :: Lens' DescribeVPCAttribute (Maybe Bool)
dvpcaDryRun = lens _dvpcaDryRun (\ s a -> s{_dvpcaDryRun = a})

-- | The VPC attribute.
dvpcaAttribute :: Lens' DescribeVPCAttribute VPCAttributeName
dvpcaAttribute = lens _dvpcaAttribute (\ s a -> s{_dvpcaAttribute = a})

-- | The ID of the VPC.
dvpcaVPCId :: Lens' DescribeVPCAttribute Text
dvpcaVPCId = lens _dvpcaVPCId (\ s a -> s{_dvpcaVPCId = a})

instance AWSRequest DescribeVPCAttribute where
        type Rs DescribeVPCAttribute =
             DescribeVPCAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCAttributeResponse' <$>
                   (x .@? "enableDnsHostnames") <*>
                     (x .@? "enableDnsSupport")
                     <*> (x .@? "vpcId")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVPCAttribute where

instance NFData DescribeVPCAttribute where

instance ToHeaders DescribeVPCAttribute where
        toHeaders = const mempty

instance ToPath DescribeVPCAttribute where
        toPath = const "/"

instance ToQuery DescribeVPCAttribute where
        toQuery DescribeVPCAttribute'{..}
          = mconcat
              ["Action" =: ("DescribeVpcAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvpcaDryRun,
               "Attribute" =: _dvpcaAttribute,
               "VpcId" =: _dvpcaVPCId]

-- | /See:/ 'describeVPCAttributeResponse' smart constructor.
data DescribeVPCAttributeResponse = DescribeVPCAttributeResponse'
  { _dvpcarsEnableDNSHostnames :: !(Maybe AttributeBooleanValue)
  , _dvpcarsEnableDNSSupport   :: !(Maybe AttributeBooleanValue)
  , _dvpcarsVPCId              :: !(Maybe Text)
  , _dvpcarsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcarsEnableDNSHostnames' - Indicates whether the instances launched in the VPC get DNS hostnames. If this attribute is @true@ , instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- * 'dvpcarsEnableDNSSupport' - Indicates whether DNS resolution is enabled for the VPC. If this attribute is @true@ , the Amazon DNS server resolves DNS hostnames for your instances to their corresponding IP addresses; otherwise, it does not.
--
-- * 'dvpcarsVPCId' - The ID of the VPC.
--
-- * 'dvpcarsResponseStatus' - -- | The response status code.
describeVPCAttributeResponse
    :: Int -- ^ 'dvpcarsResponseStatus'
    -> DescribeVPCAttributeResponse
describeVPCAttributeResponse pResponseStatus_ =
  DescribeVPCAttributeResponse'
    { _dvpcarsEnableDNSHostnames = Nothing
    , _dvpcarsEnableDNSSupport = Nothing
    , _dvpcarsVPCId = Nothing
    , _dvpcarsResponseStatus = pResponseStatus_
    }


-- | Indicates whether the instances launched in the VPC get DNS hostnames. If this attribute is @true@ , instances in the VPC get DNS hostnames; otherwise, they do not.
dvpcarsEnableDNSHostnames :: Lens' DescribeVPCAttributeResponse (Maybe AttributeBooleanValue)
dvpcarsEnableDNSHostnames = lens _dvpcarsEnableDNSHostnames (\ s a -> s{_dvpcarsEnableDNSHostnames = a})

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute is @true@ , the Amazon DNS server resolves DNS hostnames for your instances to their corresponding IP addresses; otherwise, it does not.
dvpcarsEnableDNSSupport :: Lens' DescribeVPCAttributeResponse (Maybe AttributeBooleanValue)
dvpcarsEnableDNSSupport = lens _dvpcarsEnableDNSSupport (\ s a -> s{_dvpcarsEnableDNSSupport = a})

-- | The ID of the VPC.
dvpcarsVPCId :: Lens' DescribeVPCAttributeResponse (Maybe Text)
dvpcarsVPCId = lens _dvpcarsVPCId (\ s a -> s{_dvpcarsVPCId = a})

-- | -- | The response status code.
dvpcarsResponseStatus :: Lens' DescribeVPCAttributeResponse Int
dvpcarsResponseStatus = lens _dvpcarsResponseStatus (\ s a -> s{_dvpcarsResponseStatus = a})

instance NFData DescribeVPCAttributeResponse where
