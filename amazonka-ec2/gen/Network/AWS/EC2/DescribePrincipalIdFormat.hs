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
-- Module      : Network.AWS.EC2.DescribePrincipalIdFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for the root user and all IAM roles and IAM users that have explicitly specified a longer ID (17-character ID) preference.
--
--
-- By default, all IAM roles and IAM users default to the same ID settings as the root user, unless they explicitly override the settings. This request is useful for identifying those IAM users and IAM roles that have overridden the default ID settings.
--
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ .
--
module Network.AWS.EC2.DescribePrincipalIdFormat
    (
    -- * Creating a Request
      describePrincipalIdFormat
    , DescribePrincipalIdFormat
    -- * Request Lenses
    , dpifResources
    , dpifNextToken
    , dpifDryRun
    , dpifMaxResults

    -- * Destructuring the Response
    , describePrincipalIdFormatResponse
    , DescribePrincipalIdFormatResponse
    -- * Response Lenses
    , dpifrsPrincipals
    , dpifrsNextToken
    , dpifrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePrincipalIdFormat' smart constructor.
data DescribePrincipalIdFormat = DescribePrincipalIdFormat'
  { _dpifResources  :: !(Maybe [Text])
  , _dpifNextToken  :: !(Maybe Text)
  , _dpifDryRun     :: !(Maybe Bool)
  , _dpifMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePrincipalIdFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpifResources' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
--
-- * 'dpifNextToken' - The token to request the next page of results.
--
-- * 'dpifDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dpifMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
describePrincipalIdFormat
    :: DescribePrincipalIdFormat
describePrincipalIdFormat =
  DescribePrincipalIdFormat'
    { _dpifResources = Nothing
    , _dpifNextToken = Nothing
    , _dpifDryRun = Nothing
    , _dpifMaxResults = Nothing
    }


-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@
dpifResources :: Lens' DescribePrincipalIdFormat [Text]
dpifResources = lens _dpifResources (\ s a -> s{_dpifResources = a}) . _Default . _Coerce

-- | The token to request the next page of results.
dpifNextToken :: Lens' DescribePrincipalIdFormat (Maybe Text)
dpifNextToken = lens _dpifNextToken (\ s a -> s{_dpifNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dpifDryRun :: Lens' DescribePrincipalIdFormat (Maybe Bool)
dpifDryRun = lens _dpifDryRun (\ s a -> s{_dpifDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned NextToken value.
dpifMaxResults :: Lens' DescribePrincipalIdFormat (Maybe Int)
dpifMaxResults = lens _dpifMaxResults (\ s a -> s{_dpifMaxResults = a})

instance AWSRequest DescribePrincipalIdFormat where
        type Rs DescribePrincipalIdFormat =
             DescribePrincipalIdFormatResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribePrincipalIdFormatResponse' <$>
                   (x .@? "principalSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribePrincipalIdFormat where

instance NFData DescribePrincipalIdFormat where

instance ToHeaders DescribePrincipalIdFormat where
        toHeaders = const mempty

instance ToPath DescribePrincipalIdFormat where
        toPath = const "/"

instance ToQuery DescribePrincipalIdFormat where
        toQuery DescribePrincipalIdFormat'{..}
          = mconcat
              ["Action" =:
                 ("DescribePrincipalIdFormat" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Resource" <$> _dpifResources),
               "NextToken" =: _dpifNextToken,
               "DryRun" =: _dpifDryRun,
               "MaxResults" =: _dpifMaxResults]

-- | /See:/ 'describePrincipalIdFormatResponse' smart constructor.
data DescribePrincipalIdFormatResponse = DescribePrincipalIdFormatResponse'
  { _dpifrsPrincipals     :: !(Maybe [PrincipalIdFormat])
  , _dpifrsNextToken      :: !(Maybe Text)
  , _dpifrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePrincipalIdFormatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpifrsPrincipals' - Information about the ID format settings for the ARN.
--
-- * 'dpifrsNextToken' - The token to use to retrieve the next page of results. This value is null when there are no more results to return.
--
-- * 'dpifrsResponseStatus' - -- | The response status code.
describePrincipalIdFormatResponse
    :: Int -- ^ 'dpifrsResponseStatus'
    -> DescribePrincipalIdFormatResponse
describePrincipalIdFormatResponse pResponseStatus_ =
  DescribePrincipalIdFormatResponse'
    { _dpifrsPrincipals = Nothing
    , _dpifrsNextToken = Nothing
    , _dpifrsResponseStatus = pResponseStatus_
    }


-- | Information about the ID format settings for the ARN.
dpifrsPrincipals :: Lens' DescribePrincipalIdFormatResponse [PrincipalIdFormat]
dpifrsPrincipals = lens _dpifrsPrincipals (\ s a -> s{_dpifrsPrincipals = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
dpifrsNextToken :: Lens' DescribePrincipalIdFormatResponse (Maybe Text)
dpifrsNextToken = lens _dpifrsNextToken (\ s a -> s{_dpifrsNextToken = a})

-- | -- | The response status code.
dpifrsResponseStatus :: Lens' DescribePrincipalIdFormatResponse Int
dpifrsResponseStatus = lens _dpifrsResponseStatus (\ s a -> s{_dpifrsResponseStatus = a})

instance NFData DescribePrincipalIdFormatResponse
         where
