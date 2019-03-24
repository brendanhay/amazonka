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
-- Module      : Network.AWS.EC2.ExportTransitGatewayRoutes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports routes from the specified transit gateway route table to the specified S3 bucket. By default, all routes are exported. Alternatively, you can filter by CIDR range.
--
--
module Network.AWS.EC2.ExportTransitGatewayRoutes
    (
    -- * Creating a Request
      exportTransitGatewayRoutes
    , ExportTransitGatewayRoutes
    -- * Request Lenses
    , etgrFilters
    , etgrDryRun
    , etgrTransitGatewayRouteTableId
    , etgrS3Bucket

    -- * Destructuring the Response
    , exportTransitGatewayRoutesResponse
    , ExportTransitGatewayRoutesResponse
    -- * Response Lenses
    , etgrrsS3Location
    , etgrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportTransitGatewayRoutes' smart constructor.
data ExportTransitGatewayRoutes = ExportTransitGatewayRoutes'
  { _etgrFilters                    :: !(Maybe [Filter])
  , _etgrDryRun                     :: !(Maybe Bool)
  , _etgrTransitGatewayRouteTableId :: !Text
  , _etgrS3Bucket                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportTransitGatewayRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etgrFilters' - One or more filters. The possible values are:     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.     * @attachment.resource-id@ - The resource id of the transit gateway attachment.     * @route-search.exact-match@ - The exact match of the specified filter.     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.     * @state@ - The state of the attachment (@available@ | @deleted@ | @deleting@ | @failed@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @transit-gateway-route-destination-cidr-block@ - The CIDR range.     * @type@ - The type of roue (@active@ | @blackhole@ ).
--
-- * 'etgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'etgrTransitGatewayRouteTableId' - The ID of the route table.
--
-- * 'etgrS3Bucket' - The name of the S3 bucket.
exportTransitGatewayRoutes
    :: Text -- ^ 'etgrTransitGatewayRouteTableId'
    -> Text -- ^ 'etgrS3Bucket'
    -> ExportTransitGatewayRoutes
exportTransitGatewayRoutes pTransitGatewayRouteTableId_ pS3Bucket_ =
  ExportTransitGatewayRoutes'
    { _etgrFilters = Nothing
    , _etgrDryRun = Nothing
    , _etgrTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    , _etgrS3Bucket = pS3Bucket_
    }


-- | One or more filters. The possible values are:     * @attachment.transit-gateway-attachment-id@ - The id of the transit gateway attachment.     * @attachment.resource-id@ - The resource id of the transit gateway attachment.     * @route-search.exact-match@ - The exact match of the specified filter.     * @route-search.longest-prefix-match@ - The longest prefix that matches the route.     * @route-search.subnet-of-match@ - The routes with a subnet that match the specified CIDR filter.     * @route-search.supernet-of-match@ - The routes with a CIDR that encompass the CIDR filter. For example, if you have 10.0.1.0/29 and 10.0.1.0/31 routes in your route table and you specify supernet-of-match as 10.0.1.0/30, then the result returns 10.0.1.0/29.     * @state@ - The state of the attachment (@available@ | @deleted@ | @deleting@ | @failed@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @transit-gateway-route-destination-cidr-block@ - The CIDR range.     * @type@ - The type of roue (@active@ | @blackhole@ ).
etgrFilters :: Lens' ExportTransitGatewayRoutes [Filter]
etgrFilters = lens _etgrFilters (\ s a -> s{_etgrFilters = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
etgrDryRun :: Lens' ExportTransitGatewayRoutes (Maybe Bool)
etgrDryRun = lens _etgrDryRun (\ s a -> s{_etgrDryRun = a})

-- | The ID of the route table.
etgrTransitGatewayRouteTableId :: Lens' ExportTransitGatewayRoutes Text
etgrTransitGatewayRouteTableId = lens _etgrTransitGatewayRouteTableId (\ s a -> s{_etgrTransitGatewayRouteTableId = a})

-- | The name of the S3 bucket.
etgrS3Bucket :: Lens' ExportTransitGatewayRoutes Text
etgrS3Bucket = lens _etgrS3Bucket (\ s a -> s{_etgrS3Bucket = a})

instance AWSRequest ExportTransitGatewayRoutes where
        type Rs ExportTransitGatewayRoutes =
             ExportTransitGatewayRoutesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ExportTransitGatewayRoutesResponse' <$>
                   (x .@? "s3Location") <*> (pure (fromEnum s)))

instance Hashable ExportTransitGatewayRoutes where

instance NFData ExportTransitGatewayRoutes where

instance ToHeaders ExportTransitGatewayRoutes where
        toHeaders = const mempty

instance ToPath ExportTransitGatewayRoutes where
        toPath = const "/"

instance ToQuery ExportTransitGatewayRoutes where
        toQuery ExportTransitGatewayRoutes'{..}
          = mconcat
              ["Action" =:
                 ("ExportTransitGatewayRoutes" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _etgrFilters),
               "DryRun" =: _etgrDryRun,
               "TransitGatewayRouteTableId" =:
                 _etgrTransitGatewayRouteTableId,
               "S3Bucket" =: _etgrS3Bucket]

-- | /See:/ 'exportTransitGatewayRoutesResponse' smart constructor.
data ExportTransitGatewayRoutesResponse = ExportTransitGatewayRoutesResponse'
  { _etgrrsS3Location     :: !(Maybe Text)
  , _etgrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportTransitGatewayRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etgrrsS3Location' - The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
--
-- * 'etgrrsResponseStatus' - -- | The response status code.
exportTransitGatewayRoutesResponse
    :: Int -- ^ 'etgrrsResponseStatus'
    -> ExportTransitGatewayRoutesResponse
exportTransitGatewayRoutesResponse pResponseStatus_ =
  ExportTransitGatewayRoutesResponse'
    {_etgrrsS3Location = Nothing, _etgrrsResponseStatus = pResponseStatus_}


-- | The URL of the exported file in Amazon S3. For example, s3:///bucket_name/ /VPCTransitGateway/TransitGatewayRouteTables//file_name/ .
etgrrsS3Location :: Lens' ExportTransitGatewayRoutesResponse (Maybe Text)
etgrrsS3Location = lens _etgrrsS3Location (\ s a -> s{_etgrrsS3Location = a})

-- | -- | The response status code.
etgrrsResponseStatus :: Lens' ExportTransitGatewayRoutesResponse Int
etgrrsResponseStatus = lens _etgrrsResponseStatus (\ s a -> s{_etgrrsResponseStatus = a})

instance NFData ExportTransitGatewayRoutesResponse
         where
