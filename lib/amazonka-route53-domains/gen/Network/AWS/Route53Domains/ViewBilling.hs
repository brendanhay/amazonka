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
-- Module      : Network.AWS.Route53Domains.ViewBilling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the domain-related billing records for the current AWS account for a specified period
--
--
module Network.AWS.Route53Domains.ViewBilling
    (
    -- * Creating a Request
      viewBilling
    , ViewBilling
    -- * Request Lenses
    , vbStart
    , vbEnd
    , vbMarker
    , vbMaxItems

    -- * Destructuring the Response
    , viewBillingResponse
    , ViewBillingResponse
    -- * Response Lenses
    , vbrsNextPageMarker
    , vbrsBillingRecords
    , vbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The ViewBilling request includes the following elements.
--
--
--
-- /See:/ 'viewBilling' smart constructor.
data ViewBilling = ViewBilling'
  { _vbStart    :: !(Maybe POSIX)
  , _vbEnd      :: !(Maybe POSIX)
  , _vbMarker   :: !(Maybe Text)
  , _vbMaxItems :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ViewBilling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vbStart' - The beginning date and time for the time period for which you want a list of billing records. Specify the date and time in Coordinated Universal time (UTC).
--
-- * 'vbEnd' - The end date and time for the time period for which you want a list of billing records. Specify the date and time in Coordinated Universal time (UTC).
--
-- * 'vbMarker' - For an initial request for a list of billing records, omit this element. If the number of billing records that are associated with the current AWS account during the specified period is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional billing records. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.  Constraints: The marker must match the value of @NextPageMarker@ that was returned in the previous response.
--
-- * 'vbMaxItems' - The number of billing records to be returned. Default: 20
viewBilling
    :: ViewBilling
viewBilling =
  ViewBilling'
    { _vbStart = Nothing
    , _vbEnd = Nothing
    , _vbMarker = Nothing
    , _vbMaxItems = Nothing
    }


-- | The beginning date and time for the time period for which you want a list of billing records. Specify the date and time in Coordinated Universal time (UTC).
vbStart :: Lens' ViewBilling (Maybe UTCTime)
vbStart = lens _vbStart (\ s a -> s{_vbStart = a}) . mapping _Time

-- | The end date and time for the time period for which you want a list of billing records. Specify the date and time in Coordinated Universal time (UTC).
vbEnd :: Lens' ViewBilling (Maybe UTCTime)
vbEnd = lens _vbEnd (\ s a -> s{_vbEnd = a}) . mapping _Time

-- | For an initial request for a list of billing records, omit this element. If the number of billing records that are associated with the current AWS account during the specified period is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional billing records. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.  Constraints: The marker must match the value of @NextPageMarker@ that was returned in the previous response.
vbMarker :: Lens' ViewBilling (Maybe Text)
vbMarker = lens _vbMarker (\ s a -> s{_vbMarker = a})

-- | The number of billing records to be returned. Default: 20
vbMaxItems :: Lens' ViewBilling (Maybe Int)
vbMaxItems = lens _vbMaxItems (\ s a -> s{_vbMaxItems = a})

instance AWSRequest ViewBilling where
        type Rs ViewBilling = ViewBillingResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 ViewBillingResponse' <$>
                   (x .?> "NextPageMarker") <*>
                     (x .?> "BillingRecords" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ViewBilling where

instance NFData ViewBilling where

instance ToHeaders ViewBilling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.ViewBilling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ViewBilling where
        toJSON ViewBilling'{..}
          = object
              (catMaybes
                 [("Start" .=) <$> _vbStart, ("End" .=) <$> _vbEnd,
                  ("Marker" .=) <$> _vbMarker,
                  ("MaxItems" .=) <$> _vbMaxItems])

instance ToPath ViewBilling where
        toPath = const "/"

instance ToQuery ViewBilling where
        toQuery = const mempty

-- | The ViewBilling response includes the following elements.
--
--
--
-- /See:/ 'viewBillingResponse' smart constructor.
data ViewBillingResponse = ViewBillingResponse'
  { _vbrsNextPageMarker :: !(Maybe Text)
  , _vbrsBillingRecords :: !(Maybe [BillingRecord])
  , _vbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ViewBillingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vbrsNextPageMarker' - If there are more billing records than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- * 'vbrsBillingRecords' - A summary of billing records.
--
-- * 'vbrsResponseStatus' - -- | The response status code.
viewBillingResponse
    :: Int -- ^ 'vbrsResponseStatus'
    -> ViewBillingResponse
viewBillingResponse pResponseStatus_ =
  ViewBillingResponse'
    { _vbrsNextPageMarker = Nothing
    , _vbrsBillingRecords = Nothing
    , _vbrsResponseStatus = pResponseStatus_
    }


-- | If there are more billing records than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
vbrsNextPageMarker :: Lens' ViewBillingResponse (Maybe Text)
vbrsNextPageMarker = lens _vbrsNextPageMarker (\ s a -> s{_vbrsNextPageMarker = a})

-- | A summary of billing records.
vbrsBillingRecords :: Lens' ViewBillingResponse [BillingRecord]
vbrsBillingRecords = lens _vbrsBillingRecords (\ s a -> s{_vbrsBillingRecords = a}) . _Default . _Coerce

-- | -- | The response status code.
vbrsResponseStatus :: Lens' ViewBillingResponse Int
vbrsResponseStatus = lens _vbrsResponseStatus (\ s a -> s{_vbrsResponseStatus = a})

instance NFData ViewBillingResponse where
