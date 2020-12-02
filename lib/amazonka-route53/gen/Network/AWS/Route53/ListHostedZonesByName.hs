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
-- Module      : Network.AWS.Route53.ListHostedZonesByName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of your hosted zones in lexicographic order. The response includes a @HostedZones@ child element for each hosted zone created by the current AWS account.
--
--
-- @ListHostedZonesByName@ sorts hosted zones by name with the labels reversed. For example:
--
-- @com.example.www.@
--
-- Note the trailing dot, which can change the sort order in some circumstances.
--
-- If the domain name includes escape characters or Punycode, @ListHostedZonesByName@ alphabetizes the domain name using the escaped or Punycoded value, which is the format that Amazon Route 53 saves in its database. For example, to create a hosted zone for exämple.com, you specify ex\344mple.com for the domain name. @ListHostedZonesByName@ alphabetizes it as:
--
-- @com.ex\344mple.@
--
-- The labels are reversed and alphabetized using the escaped value. For more information about valid domain name formats, including internationalized domain names, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
--
-- Amazon Route 53 returns up to 100 items in each response. If you have a lot of hosted zones, use the @MaxItems@ parameter to list them in groups of up to 100. The response includes values that help navigate from one group of @MaxItems@ hosted zones to the next:
--
--     * The @DNSName@ and @HostedZoneId@ elements in the response contain the values, if any, specified for the @dnsname@ and @hostedzoneid@ parameters in the request that produced the current response.
--
--     * The @MaxItems@ element in the response contains the value, if any, that you specified for the @maxitems@ parameter in the request that produced the current response.
--
--     * If the value of @IsTruncated@ in the response is true, there are more hosted zones associated with the current AWS account.
--
-- If @IsTruncated@ is false, this response includes the last hosted zone that is associated with the current account. The @NextDNSName@ element and @NextHostedZoneId@ elements are omitted from the response.
--
--     * The @NextDNSName@ and @NextHostedZoneId@ elements in the response contain the domain name and the hosted zone ID of the next hosted zone that is associated with the current AWS account. If you want to list more hosted zones, make another call to @ListHostedZonesByName@ , and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
--
--
module Network.AWS.Route53.ListHostedZonesByName
    (
    -- * Creating a Request
      listHostedZonesByName
    , ListHostedZonesByName
    -- * Request Lenses
    , lhzbnHostedZoneId
    , lhzbnMaxItems
    , lhzbnDNSName

    -- * Destructuring the Response
    , listHostedZonesByNameResponse
    , ListHostedZonesByNameResponse
    -- * Response Lenses
    , lhzbnrsHostedZoneId
    , lhzbnrsNextHostedZoneId
    , lhzbnrsDNSName
    , lhzbnrsNextDNSName
    , lhzbnrsResponseStatus
    , lhzbnrsHostedZones
    , lhzbnrsIsTruncated
    , lhzbnrsMaxItems
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | Retrieves a list of the public and private hosted zones that are associated with the current AWS account in ASCII order by domain name.
--
--
--
-- /See:/ 'listHostedZonesByName' smart constructor.
data ListHostedZonesByName = ListHostedZonesByName'
  { _lhzbnHostedZoneId :: !(Maybe ResourceId)
  , _lhzbnMaxItems     :: !(Maybe Text)
  , _lhzbnDNSName      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHostedZonesByName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzbnHostedZoneId' - (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter. If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
--
-- * 'lhzbnMaxItems' - The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
--
-- * 'lhzbnDNSName' - (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
listHostedZonesByName
    :: ListHostedZonesByName
listHostedZonesByName =
  ListHostedZonesByName'
    { _lhzbnHostedZoneId = Nothing
    , _lhzbnMaxItems = Nothing
    , _lhzbnDNSName = Nothing
    }


-- | (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter. If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
lhzbnHostedZoneId :: Lens' ListHostedZonesByName (Maybe ResourceId)
lhzbnHostedZoneId = lens _lhzbnHostedZoneId (\ s a -> s{_lhzbnHostedZoneId = a})

-- | The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
lhzbnMaxItems :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnMaxItems = lens _lhzbnMaxItems (\ s a -> s{_lhzbnMaxItems = a})

-- | (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
lhzbnDNSName :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnDNSName = lens _lhzbnDNSName (\ s a -> s{_lhzbnDNSName = a})

instance AWSRequest ListHostedZonesByName where
        type Rs ListHostedZonesByName =
             ListHostedZonesByNameResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListHostedZonesByNameResponse' <$>
                   (x .@? "HostedZoneId") <*> (x .@? "NextHostedZoneId")
                     <*> (x .@? "DNSName")
                     <*> (x .@? "NextDNSName")
                     <*> (pure (fromEnum s))
                     <*>
                     (x .@? "HostedZones" .!@ mempty >>=
                        parseXMLList "HostedZone")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance Hashable ListHostedZonesByName where

instance NFData ListHostedZonesByName where

instance ToHeaders ListHostedZonesByName where
        toHeaders = const mempty

instance ToPath ListHostedZonesByName where
        toPath = const "/2013-04-01/hostedzonesbyname"

instance ToQuery ListHostedZonesByName where
        toQuery ListHostedZonesByName'{..}
          = mconcat
              ["hostedzoneid" =: _lhzbnHostedZoneId,
               "maxitems" =: _lhzbnMaxItems,
               "dnsname" =: _lhzbnDNSName]

-- | A complex type that contains the response information for the request.
--
--
--
-- /See:/ 'listHostedZonesByNameResponse' smart constructor.
data ListHostedZonesByNameResponse = ListHostedZonesByNameResponse'
  { _lhzbnrsHostedZoneId     :: !(Maybe ResourceId)
  , _lhzbnrsNextHostedZoneId :: !(Maybe ResourceId)
  , _lhzbnrsDNSName          :: !(Maybe Text)
  , _lhzbnrsNextDNSName      :: !(Maybe Text)
  , _lhzbnrsResponseStatus   :: !Int
  , _lhzbnrsHostedZones      :: ![HostedZone]
  , _lhzbnrsIsTruncated      :: !Bool
  , _lhzbnrsMaxItems         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHostedZonesByNameResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhzbnrsHostedZoneId' - The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- * 'lhzbnrsNextHostedZoneId' - If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively. This element is present only if @IsTruncated@ is @true@ .
--
-- * 'lhzbnrsDNSName' - For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
--
-- * 'lhzbnrsNextDNSName' - If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively. This element is present only if @IsTruncated@ is @true@ .
--
-- * 'lhzbnrsResponseStatus' - -- | The response status code.
--
-- * 'lhzbnrsHostedZones' - A complex type that contains general information about the hosted zone.
--
-- * 'lhzbnrsIsTruncated' - A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
--
-- * 'lhzbnrsMaxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
listHostedZonesByNameResponse
    :: Int -- ^ 'lhzbnrsResponseStatus'
    -> Bool -- ^ 'lhzbnrsIsTruncated'
    -> Text -- ^ 'lhzbnrsMaxItems'
    -> ListHostedZonesByNameResponse
listHostedZonesByNameResponse pResponseStatus_ pIsTruncated_ pMaxItems_ =
  ListHostedZonesByNameResponse'
    { _lhzbnrsHostedZoneId = Nothing
    , _lhzbnrsNextHostedZoneId = Nothing
    , _lhzbnrsDNSName = Nothing
    , _lhzbnrsNextDNSName = Nothing
    , _lhzbnrsResponseStatus = pResponseStatus_
    , _lhzbnrsHostedZones = mempty
    , _lhzbnrsIsTruncated = pIsTruncated_
    , _lhzbnrsMaxItems = pMaxItems_
    }


-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
lhzbnrsHostedZoneId :: Lens' ListHostedZonesByNameResponse (Maybe ResourceId)
lhzbnrsHostedZoneId = lens _lhzbnrsHostedZoneId (\ s a -> s{_lhzbnrsHostedZoneId = a})

-- | If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively. This element is present only if @IsTruncated@ is @true@ .
lhzbnrsNextHostedZoneId :: Lens' ListHostedZonesByNameResponse (Maybe ResourceId)
lhzbnrsNextHostedZoneId = lens _lhzbnrsNextHostedZoneId (\ s a -> s{_lhzbnrsNextHostedZoneId = a})

-- | For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
lhzbnrsDNSName :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrsDNSName = lens _lhzbnrsDNSName (\ s a -> s{_lhzbnrsDNSName = a})

-- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively. This element is present only if @IsTruncated@ is @true@ .
lhzbnrsNextDNSName :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrsNextDNSName = lens _lhzbnrsNextDNSName (\ s a -> s{_lhzbnrsNextDNSName = a})

-- | -- | The response status code.
lhzbnrsResponseStatus :: Lens' ListHostedZonesByNameResponse Int
lhzbnrsResponseStatus = lens _lhzbnrsResponseStatus (\ s a -> s{_lhzbnrsResponseStatus = a})

-- | A complex type that contains general information about the hosted zone.
lhzbnrsHostedZones :: Lens' ListHostedZonesByNameResponse [HostedZone]
lhzbnrsHostedZones = lens _lhzbnrsHostedZones (\ s a -> s{_lhzbnrsHostedZones = a}) . _Coerce

-- | A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
lhzbnrsIsTruncated :: Lens' ListHostedZonesByNameResponse Bool
lhzbnrsIsTruncated = lens _lhzbnrsIsTruncated (\ s a -> s{_lhzbnrsIsTruncated = a})

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
lhzbnrsMaxItems :: Lens' ListHostedZonesByNameResponse Text
lhzbnrsMaxItems = lens _lhzbnrsMaxItems (\ s a -> s{_lhzbnrsMaxItems = a})

instance NFData ListHostedZonesByNameResponse where
