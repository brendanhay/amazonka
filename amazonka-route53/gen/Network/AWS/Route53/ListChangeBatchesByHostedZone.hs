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
-- Module      : Network.AWS.Route53.ListChangeBatchesByHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action gets the list of ChangeBatches in a given time period for a
-- given hosted zone.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListChangeBatchesByHostedZone.html AWS API Reference> for ListChangeBatchesByHostedZone.
module Network.AWS.Route53.ListChangeBatchesByHostedZone
    (
    -- * Creating a Request
      listChangeBatchesByHostedZone
    , ListChangeBatchesByHostedZone
    -- * Request Lenses
    , lcbbhzMarker
    , lcbbhzMaxItems
    , lcbbhzHostedZoneId
    , lcbbhzStartDate
    , lcbbhzEndDate

    -- * Destructuring the Response
    , listChangeBatchesByHostedZoneResponse
    , ListChangeBatchesByHostedZoneResponse
    -- * Response Lenses
    , lcbbhzrsNextMarker
    , lcbbhzrsIsTruncated
    , lcbbhzrsResponseStatus
    , lcbbhzrsMaxItems
    , lcbbhzrsMarker
    , lcbbhzrsChangeBatchRecords
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a ListChangeBatchesByHostedZone request.
--
-- /See:/ 'listChangeBatchesByHostedZone' smart constructor.
data ListChangeBatchesByHostedZone = ListChangeBatchesByHostedZone'
    { _lcbbhzMarker       :: !(Maybe Text)
    , _lcbbhzMaxItems     :: !(Maybe Text)
    , _lcbbhzHostedZoneId :: !Text
    , _lcbbhzStartDate    :: !Text
    , _lcbbhzEndDate      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListChangeBatchesByHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcbbhzMarker'
--
-- * 'lcbbhzMaxItems'
--
-- * 'lcbbhzHostedZoneId'
--
-- * 'lcbbhzStartDate'
--
-- * 'lcbbhzEndDate'
listChangeBatchesByHostedZone
    :: Text -- ^ 'lcbbhzHostedZoneId'
    -> Text -- ^ 'lcbbhzStartDate'
    -> Text -- ^ 'lcbbhzEndDate'
    -> ListChangeBatchesByHostedZone
listChangeBatchesByHostedZone pHostedZoneId_ pStartDate_ pEndDate_ =
    ListChangeBatchesByHostedZone'
    { _lcbbhzMarker = Nothing
    , _lcbbhzMaxItems = Nothing
    , _lcbbhzHostedZoneId = pHostedZoneId_
    , _lcbbhzStartDate = pStartDate_
    , _lcbbhzEndDate = pEndDate_
    }

-- | The page marker.
lcbbhzMarker :: Lens' ListChangeBatchesByHostedZone (Maybe Text)
lcbbhzMarker = lens _lcbbhzMarker (\ s a -> s{_lcbbhzMarker = a});

-- | The maximum number of items on a page.
lcbbhzMaxItems :: Lens' ListChangeBatchesByHostedZone (Maybe Text)
lcbbhzMaxItems = lens _lcbbhzMaxItems (\ s a -> s{_lcbbhzMaxItems = a});

-- | The ID of the hosted zone that you want to see changes for.
lcbbhzHostedZoneId :: Lens' ListChangeBatchesByHostedZone Text
lcbbhzHostedZoneId = lens _lcbbhzHostedZoneId (\ s a -> s{_lcbbhzHostedZoneId = a});

-- | The start of the time period you want to see changes for.
lcbbhzStartDate :: Lens' ListChangeBatchesByHostedZone Text
lcbbhzStartDate = lens _lcbbhzStartDate (\ s a -> s{_lcbbhzStartDate = a});

-- | The end of the time period you want to see changes for.
lcbbhzEndDate :: Lens' ListChangeBatchesByHostedZone Text
lcbbhzEndDate = lens _lcbbhzEndDate (\ s a -> s{_lcbbhzEndDate = a});

instance AWSRequest ListChangeBatchesByHostedZone
         where
        type Rs ListChangeBatchesByHostedZone =
             ListChangeBatchesByHostedZoneResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListChangeBatchesByHostedZoneResponse' <$>
                   (x .@? "NextMarker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*> (x .@ "MaxItems")
                     <*> (x .@ "Marker")
                     <*>
                     (x .@? "ChangeBatchRecords" .!@ mempty >>=
                        parseXMLList1 "ChangeBatchRecord"))

instance ToHeaders ListChangeBatchesByHostedZone
         where
        toHeaders = const mempty

instance ToPath ListChangeBatchesByHostedZone where
        toPath ListChangeBatchesByHostedZone'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toBS _lcbbhzHostedZoneId,
               "/changes"]

instance ToQuery ListChangeBatchesByHostedZone where
        toQuery ListChangeBatchesByHostedZone'{..}
          = mconcat
              ["marker" =: _lcbbhzMarker,
               "maxItems" =: _lcbbhzMaxItems,
               "startDate" =: _lcbbhzStartDate,
               "endDate" =: _lcbbhzEndDate]

-- | The input for a ListChangeBatchesByHostedZone request.
--
-- /See:/ 'listChangeBatchesByHostedZoneResponse' smart constructor.
data ListChangeBatchesByHostedZoneResponse = ListChangeBatchesByHostedZoneResponse'
    { _lcbbhzrsNextMarker         :: !(Maybe Text)
    , _lcbbhzrsIsTruncated        :: !(Maybe Bool)
    , _lcbbhzrsResponseStatus     :: !Int
    , _lcbbhzrsMaxItems           :: !Text
    , _lcbbhzrsMarker             :: !Text
    , _lcbbhzrsChangeBatchRecords :: !(List1 ChangeBatchRecord)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListChangeBatchesByHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcbbhzrsNextMarker'
--
-- * 'lcbbhzrsIsTruncated'
--
-- * 'lcbbhzrsResponseStatus'
--
-- * 'lcbbhzrsMaxItems'
--
-- * 'lcbbhzrsMarker'
--
-- * 'lcbbhzrsChangeBatchRecords'
listChangeBatchesByHostedZoneResponse
    :: Int -- ^ 'lcbbhzrsResponseStatus'
    -> Text -- ^ 'lcbbhzrsMaxItems'
    -> Text -- ^ 'lcbbhzrsMarker'
    -> NonEmpty ChangeBatchRecord -- ^ 'lcbbhzrsChangeBatchRecords'
    -> ListChangeBatchesByHostedZoneResponse
listChangeBatchesByHostedZoneResponse pResponseStatus_ pMaxItems_ pMarker_ pChangeBatchRecords_ =
    ListChangeBatchesByHostedZoneResponse'
    { _lcbbhzrsNextMarker = Nothing
    , _lcbbhzrsIsTruncated = Nothing
    , _lcbbhzrsResponseStatus = pResponseStatus_
    , _lcbbhzrsMaxItems = pMaxItems_
    , _lcbbhzrsMarker = pMarker_
    , _lcbbhzrsChangeBatchRecords = _List1 # pChangeBatchRecords_
    }

-- | The next page marker.
lcbbhzrsNextMarker :: Lens' ListChangeBatchesByHostedZoneResponse (Maybe Text)
lcbbhzrsNextMarker = lens _lcbbhzrsNextMarker (\ s a -> s{_lcbbhzrsNextMarker = a});

-- | A flag that indicates if there are more change batches to list.
lcbbhzrsIsTruncated :: Lens' ListChangeBatchesByHostedZoneResponse (Maybe Bool)
lcbbhzrsIsTruncated = lens _lcbbhzrsIsTruncated (\ s a -> s{_lcbbhzrsIsTruncated = a});

-- | The response status code.
lcbbhzrsResponseStatus :: Lens' ListChangeBatchesByHostedZoneResponse Int
lcbbhzrsResponseStatus = lens _lcbbhzrsResponseStatus (\ s a -> s{_lcbbhzrsResponseStatus = a});

-- | The maximum number of items on a page.
lcbbhzrsMaxItems :: Lens' ListChangeBatchesByHostedZoneResponse Text
lcbbhzrsMaxItems = lens _lcbbhzrsMaxItems (\ s a -> s{_lcbbhzrsMaxItems = a});

-- | The page marker.
lcbbhzrsMarker :: Lens' ListChangeBatchesByHostedZoneResponse Text
lcbbhzrsMarker = lens _lcbbhzrsMarker (\ s a -> s{_lcbbhzrsMarker = a});

-- | The change batches within the given hosted zone and time period.
lcbbhzrsChangeBatchRecords :: Lens' ListChangeBatchesByHostedZoneResponse (NonEmpty ChangeBatchRecord)
lcbbhzrsChangeBatchRecords = lens _lcbbhzrsChangeBatchRecords (\ s a -> s{_lcbbhzrsChangeBatchRecords = a}) . _List1;
