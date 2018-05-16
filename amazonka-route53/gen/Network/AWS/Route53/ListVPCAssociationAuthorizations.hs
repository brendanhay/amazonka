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
-- Module      : Network.AWS.Route53.ListVPCAssociationAuthorizations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the VPCs that were created by other accounts and that can be associated with a specified hosted zone because you've submitted one or more @CreateVPCAssociationAuthorization@ requests.
--
--
-- The response includes a @VPCs@ element with a @VPC@ child element for each VPC that can be associated with the hosted zone.
--
module Network.AWS.Route53.ListVPCAssociationAuthorizations
    (
    -- * Creating a Request
      listVPCAssociationAuthorizations
    , ListVPCAssociationAuthorizations
    -- * Request Lenses
    , lvaaNextToken
    , lvaaMaxResults
    , lvaaHostedZoneId

    -- * Destructuring the Response
    , listVPCAssociationAuthorizationsResponse
    , ListVPCAssociationAuthorizationsResponse
    -- * Response Lenses
    , lvaarsNextToken
    , lvaarsResponseStatus
    , lvaarsHostedZoneId
    , lvaarsVPCs
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about that can be associated with your hosted zone.
--
--
--
-- /See:/ 'listVPCAssociationAuthorizations' smart constructor.
data ListVPCAssociationAuthorizations = ListVPCAssociationAuthorizations'
  { _lvaaNextToken    :: !(Maybe Text)
  , _lvaaMaxResults   :: !(Maybe Text)
  , _lvaaHostedZoneId :: !ResourceId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVPCAssociationAuthorizations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvaaNextToken' - /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
--
-- * 'lvaaMaxResults' - /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Amazon Route 53 returns up to 50 VPCs per page.
--
-- * 'lvaaHostedZoneId' - The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
listVPCAssociationAuthorizations
    :: ResourceId -- ^ 'lvaaHostedZoneId'
    -> ListVPCAssociationAuthorizations
listVPCAssociationAuthorizations pHostedZoneId_ =
  ListVPCAssociationAuthorizations'
    { _lvaaNextToken = Nothing
    , _lvaaMaxResults = Nothing
    , _lvaaHostedZoneId = pHostedZoneId_
    }


-- | /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
lvaaNextToken :: Lens' ListVPCAssociationAuthorizations (Maybe Text)
lvaaNextToken = lens _lvaaNextToken (\ s a -> s{_lvaaNextToken = a})

-- | /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Amazon Route 53 returns up to 50 VPCs per page.
lvaaMaxResults :: Lens' ListVPCAssociationAuthorizations (Maybe Text)
lvaaMaxResults = lens _lvaaMaxResults (\ s a -> s{_lvaaMaxResults = a})

-- | The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
lvaaHostedZoneId :: Lens' ListVPCAssociationAuthorizations ResourceId
lvaaHostedZoneId = lens _lvaaHostedZoneId (\ s a -> s{_lvaaHostedZoneId = a})

instance AWSRequest ListVPCAssociationAuthorizations
         where
        type Rs ListVPCAssociationAuthorizations =
             ListVPCAssociationAuthorizationsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListVPCAssociationAuthorizationsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@ "HostedZoneId")
                     <*>
                     (x .@? "VPCs" .!@ mempty >>= parseXMLList1 "VPC"))

instance Hashable ListVPCAssociationAuthorizations
         where

instance NFData ListVPCAssociationAuthorizations
         where

instance ToHeaders ListVPCAssociationAuthorizations
         where
        toHeaders = const mempty

instance ToPath ListVPCAssociationAuthorizations
         where
        toPath ListVPCAssociationAuthorizations'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toBS _lvaaHostedZoneId,
               "/authorizevpcassociation"]

instance ToQuery ListVPCAssociationAuthorizations
         where
        toQuery ListVPCAssociationAuthorizations'{..}
          = mconcat
              ["nexttoken" =: _lvaaNextToken,
               "maxresults" =: _lvaaMaxResults]

-- | A complex type that contains the response information for the request.
--
--
--
-- /See:/ 'listVPCAssociationAuthorizationsResponse' smart constructor.
data ListVPCAssociationAuthorizationsResponse = ListVPCAssociationAuthorizationsResponse'
  { _lvaarsNextToken      :: !(Maybe Text)
  , _lvaarsResponseStatus :: !Int
  , _lvaarsHostedZoneId   :: !ResourceId
  , _lvaarsVPCs           :: !(List1 VPC)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVPCAssociationAuthorizationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvaarsNextToken' - When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
--
-- * 'lvaarsResponseStatus' - -- | The response status code.
--
-- * 'lvaarsHostedZoneId' - The ID of the hosted zone that you can associate the listed VPCs with.
--
-- * 'lvaarsVPCs' - The list of VPCs that are authorized to be associated with the specified hosted zone.
listVPCAssociationAuthorizationsResponse
    :: Int -- ^ 'lvaarsResponseStatus'
    -> ResourceId -- ^ 'lvaarsHostedZoneId'
    -> NonEmpty VPC -- ^ 'lvaarsVPCs'
    -> ListVPCAssociationAuthorizationsResponse
listVPCAssociationAuthorizationsResponse pResponseStatus_ pHostedZoneId_ pVPCs_ =
  ListVPCAssociationAuthorizationsResponse'
    { _lvaarsNextToken = Nothing
    , _lvaarsResponseStatus = pResponseStatus_
    , _lvaarsHostedZoneId = pHostedZoneId_
    , _lvaarsVPCs = _List1 # pVPCs_
    }


-- | When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
lvaarsNextToken :: Lens' ListVPCAssociationAuthorizationsResponse (Maybe Text)
lvaarsNextToken = lens _lvaarsNextToken (\ s a -> s{_lvaarsNextToken = a})

-- | -- | The response status code.
lvaarsResponseStatus :: Lens' ListVPCAssociationAuthorizationsResponse Int
lvaarsResponseStatus = lens _lvaarsResponseStatus (\ s a -> s{_lvaarsResponseStatus = a})

-- | The ID of the hosted zone that you can associate the listed VPCs with.
lvaarsHostedZoneId :: Lens' ListVPCAssociationAuthorizationsResponse ResourceId
lvaarsHostedZoneId = lens _lvaarsHostedZoneId (\ s a -> s{_lvaarsHostedZoneId = a})

-- | The list of VPCs that are authorized to be associated with the specified hosted zone.
lvaarsVPCs :: Lens' ListVPCAssociationAuthorizationsResponse (NonEmpty VPC)
lvaarsVPCs = lens _lvaarsVPCs (\ s a -> s{_lvaarsVPCs = a}) . _List1

instance NFData
           ListVPCAssociationAuthorizationsResponse
         where
