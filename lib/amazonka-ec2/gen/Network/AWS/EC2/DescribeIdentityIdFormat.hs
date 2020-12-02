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
-- Module      : Network.AWS.EC2.DescribeIdentityIdFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for resources for the specified IAM user, IAM role, or root user. For example, you can view the resource types that are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
-- These settings apply to the principal specified in the request. They do not apply to the principal that makes the request.
--
module Network.AWS.EC2.DescribeIdentityIdFormat
    (
    -- * Creating a Request
      describeIdentityIdFormat
    , DescribeIdentityIdFormat
    -- * Request Lenses
    , diifResource
    , diifPrincipalARN

    -- * Destructuring the Response
    , describeIdentityIdFormatResponse
    , DescribeIdentityIdFormatResponse
    -- * Response Lenses
    , diifrsStatuses
    , diifrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeIdentityIdFormat.
--
--
--
-- /See:/ 'describeIdentityIdFormat' smart constructor.
data DescribeIdentityIdFormat = DescribeIdentityIdFormat'
  { _diifResource     :: !(Maybe Text)
  , _diifPrincipalARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIdentityIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diifResource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
--
-- * 'diifPrincipalARN' - The ARN of the principal, which can be an IAM role, IAM user, or the root user.
describeIdentityIdFormat
    :: Text -- ^ 'diifPrincipalARN'
    -> DescribeIdentityIdFormat
describeIdentityIdFormat pPrincipalARN_ =
  DescribeIdentityIdFormat'
    {_diifResource = Nothing, _diifPrincipalARN = pPrincipalARN_}


-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
diifResource :: Lens' DescribeIdentityIdFormat (Maybe Text)
diifResource = lens _diifResource (\ s a -> s{_diifResource = a})

-- | The ARN of the principal, which can be an IAM role, IAM user, or the root user.
diifPrincipalARN :: Lens' DescribeIdentityIdFormat Text
diifPrincipalARN = lens _diifPrincipalARN (\ s a -> s{_diifPrincipalARN = a})

instance AWSRequest DescribeIdentityIdFormat where
        type Rs DescribeIdentityIdFormat =
             DescribeIdentityIdFormatResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeIdentityIdFormatResponse' <$>
                   (x .@? "statusSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeIdentityIdFormat where

instance NFData DescribeIdentityIdFormat where

instance ToHeaders DescribeIdentityIdFormat where
        toHeaders = const mempty

instance ToPath DescribeIdentityIdFormat where
        toPath = const "/"

instance ToQuery DescribeIdentityIdFormat where
        toQuery DescribeIdentityIdFormat'{..}
          = mconcat
              ["Action" =:
                 ("DescribeIdentityIdFormat" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Resource" =: _diifResource,
               "PrincipalArn" =: _diifPrincipalARN]

-- | Contains the output of DescribeIdentityIdFormat.
--
--
--
-- /See:/ 'describeIdentityIdFormatResponse' smart constructor.
data DescribeIdentityIdFormatResponse = DescribeIdentityIdFormatResponse'
  { _diifrsStatuses       :: !(Maybe [IdFormat])
  , _diifrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIdentityIdFormatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diifrsStatuses' - Information about the ID format for the resources.
--
-- * 'diifrsResponseStatus' - -- | The response status code.
describeIdentityIdFormatResponse
    :: Int -- ^ 'diifrsResponseStatus'
    -> DescribeIdentityIdFormatResponse
describeIdentityIdFormatResponse pResponseStatus_ =
  DescribeIdentityIdFormatResponse'
    {_diifrsStatuses = Nothing, _diifrsResponseStatus = pResponseStatus_}


-- | Information about the ID format for the resources.
diifrsStatuses :: Lens' DescribeIdentityIdFormatResponse [IdFormat]
diifrsStatuses = lens _diifrsStatuses (\ s a -> s{_diifrsStatuses = a}) . _Default . _Coerce

-- | -- | The response status code.
diifrsResponseStatus :: Lens' DescribeIdentityIdFormatResponse Int
diifrsResponseStatus = lens _diifrsResponseStatus (\ s a -> s{_diifrsResponseStatus = a})

instance NFData DescribeIdentityIdFormatResponse
         where
