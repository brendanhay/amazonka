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
-- Module      : Network.AWS.Route53.ListChangeBatchesByRRSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action gets the list of ChangeBatches in a given time period for a
-- given hosted zone and RRSet.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListChangeBatchesByRRSet.html AWS API Reference> for ListChangeBatchesByRRSet.
module Network.AWS.Route53.ListChangeBatchesByRRSet
    (
    -- * Creating a Request
      listChangeBatchesByRRSet
    , ListChangeBatchesByRRSet
    -- * Request Lenses
    , lcbbrrsSetIdentifier
    , lcbbrrsMarker
    , lcbbrrsMaxItems
    , lcbbrrsHostedZoneId
    , lcbbrrsName
    , lcbbrrsType
    , lcbbrrsStartDate
    , lcbbrrsEndDate

    -- * Destructuring the Response
    , listChangeBatchesByRRSetResponse
    , ListChangeBatchesByRRSetResponse
    -- * Response Lenses
    , lcbbrrsrsNextMarker
    , lcbbrrsrsIsTruncated
    , lcbbrrsrsResponseStatus
    , lcbbrrsrsMaxItems
    , lcbbrrsrsMarker
    , lcbbrrsrsChangeBatchRecords
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a ListChangeBatchesByRRSet request.
--
-- /See:/ 'listChangeBatchesByRRSet' smart constructor.
data ListChangeBatchesByRRSet = ListChangeBatchesByRRSet'
    { _lcbbrrsSetIdentifier :: !(Maybe Text)
    , _lcbbrrsMarker        :: !(Maybe Text)
    , _lcbbrrsMaxItems      :: !(Maybe Text)
    , _lcbbrrsHostedZoneId  :: !Text
    , _lcbbrrsName          :: !Text
    , _lcbbrrsType          :: !RecordType
    , _lcbbrrsStartDate     :: !Text
    , _lcbbrrsEndDate       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListChangeBatchesByRRSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcbbrrsSetIdentifier'
--
-- * 'lcbbrrsMarker'
--
-- * 'lcbbrrsMaxItems'
--
-- * 'lcbbrrsHostedZoneId'
--
-- * 'lcbbrrsName'
--
-- * 'lcbbrrsType'
--
-- * 'lcbbrrsStartDate'
--
-- * 'lcbbrrsEndDate'
listChangeBatchesByRRSet
    :: Text -- ^ 'lcbbrrsHostedZoneId'
    -> Text -- ^ 'lcbbrrsName'
    -> RecordType -- ^ 'lcbbrrsType'
    -> Text -- ^ 'lcbbrrsStartDate'
    -> Text -- ^ 'lcbbrrsEndDate'
    -> ListChangeBatchesByRRSet
listChangeBatchesByRRSet pHostedZoneId_ pName_ pType_ pStartDate_ pEndDate_ =
    ListChangeBatchesByRRSet'
    { _lcbbrrsSetIdentifier = Nothing
    , _lcbbrrsMarker = Nothing
    , _lcbbrrsMaxItems = Nothing
    , _lcbbrrsHostedZoneId = pHostedZoneId_
    , _lcbbrrsName = pName_
    , _lcbbrrsType = pType_
    , _lcbbrrsStartDate = pStartDate_
    , _lcbbrrsEndDate = pEndDate_
    }

-- | The identifier of the RRSet that you want to see changes for.
lcbbrrsSetIdentifier :: Lens' ListChangeBatchesByRRSet (Maybe Text)
lcbbrrsSetIdentifier = lens _lcbbrrsSetIdentifier (\ s a -> s{_lcbbrrsSetIdentifier = a});

-- | The page marker.
lcbbrrsMarker :: Lens' ListChangeBatchesByRRSet (Maybe Text)
lcbbrrsMarker = lens _lcbbrrsMarker (\ s a -> s{_lcbbrrsMarker = a});

-- | The maximum number of items on a page.
lcbbrrsMaxItems :: Lens' ListChangeBatchesByRRSet (Maybe Text)
lcbbrrsMaxItems = lens _lcbbrrsMaxItems (\ s a -> s{_lcbbrrsMaxItems = a});

-- | The ID of the hosted zone that you want to see changes for.
lcbbrrsHostedZoneId :: Lens' ListChangeBatchesByRRSet Text
lcbbrrsHostedZoneId = lens _lcbbrrsHostedZoneId (\ s a -> s{_lcbbrrsHostedZoneId = a});

-- | The name of the RRSet that you want to see changes for.
lcbbrrsName :: Lens' ListChangeBatchesByRRSet Text
lcbbrrsName = lens _lcbbrrsName (\ s a -> s{_lcbbrrsName = a});

-- | The type of the RRSet that you want to see changes for.
lcbbrrsType :: Lens' ListChangeBatchesByRRSet RecordType
lcbbrrsType = lens _lcbbrrsType (\ s a -> s{_lcbbrrsType = a});

-- | The start of the time period you want to see changes for.
lcbbrrsStartDate :: Lens' ListChangeBatchesByRRSet Text
lcbbrrsStartDate = lens _lcbbrrsStartDate (\ s a -> s{_lcbbrrsStartDate = a});

-- | The end of the time period you want to see changes for.
lcbbrrsEndDate :: Lens' ListChangeBatchesByRRSet Text
lcbbrrsEndDate = lens _lcbbrrsEndDate (\ s a -> s{_lcbbrrsEndDate = a});

instance AWSRequest ListChangeBatchesByRRSet where
        type Rs ListChangeBatchesByRRSet =
             ListChangeBatchesByRRSetResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListChangeBatchesByRRSetResponse' <$>
                   (x .@? "NextMarker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*> (x .@ "MaxItems")
                     <*> (x .@ "Marker")
                     <*>
                     (x .@? "ChangeBatchRecords" .!@ mempty >>=
                        parseXMLList1 "ChangeBatchRecord"))

instance ToHeaders ListChangeBatchesByRRSet where
        toHeaders = const mempty

instance ToPath ListChangeBatchesByRRSet where
        toPath ListChangeBatchesByRRSet'{..}
          = mconcat
              ["/2013-04-01/hostedzone/",
               toBS _lcbbrrsHostedZoneId, "/rrsChanges"]

instance ToQuery ListChangeBatchesByRRSet where
        toQuery ListChangeBatchesByRRSet'{..}
          = mconcat
              ["identifier" =: _lcbbrrsSetIdentifier,
               "marker" =: _lcbbrrsMarker,
               "maxItems" =: _lcbbrrsMaxItems,
               "rrSet_name" =: _lcbbrrsName, "type" =: _lcbbrrsType,
               "startDate" =: _lcbbrrsStartDate,
               "endDate" =: _lcbbrrsEndDate]

-- | The input for a ListChangeBatchesByRRSet request.
--
-- /See:/ 'listChangeBatchesByRRSetResponse' smart constructor.
data ListChangeBatchesByRRSetResponse = ListChangeBatchesByRRSetResponse'
    { _lcbbrrsrsNextMarker         :: !(Maybe Text)
    , _lcbbrrsrsIsTruncated        :: !(Maybe Bool)
    , _lcbbrrsrsResponseStatus     :: !Int
    , _lcbbrrsrsMaxItems           :: !Text
    , _lcbbrrsrsMarker             :: !Text
    , _lcbbrrsrsChangeBatchRecords :: !(List1 ChangeBatchRecord)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListChangeBatchesByRRSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcbbrrsrsNextMarker'
--
-- * 'lcbbrrsrsIsTruncated'
--
-- * 'lcbbrrsrsResponseStatus'
--
-- * 'lcbbrrsrsMaxItems'
--
-- * 'lcbbrrsrsMarker'
--
-- * 'lcbbrrsrsChangeBatchRecords'
listChangeBatchesByRRSetResponse
    :: Int -- ^ 'lcbbrrsrsResponseStatus'
    -> Text -- ^ 'lcbbrrsrsMaxItems'
    -> Text -- ^ 'lcbbrrsrsMarker'
    -> NonEmpty ChangeBatchRecord -- ^ 'lcbbrrsrsChangeBatchRecords'
    -> ListChangeBatchesByRRSetResponse
listChangeBatchesByRRSetResponse pResponseStatus_ pMaxItems_ pMarker_ pChangeBatchRecords_ =
    ListChangeBatchesByRRSetResponse'
    { _lcbbrrsrsNextMarker = Nothing
    , _lcbbrrsrsIsTruncated = Nothing
    , _lcbbrrsrsResponseStatus = pResponseStatus_
    , _lcbbrrsrsMaxItems = pMaxItems_
    , _lcbbrrsrsMarker = pMarker_
    , _lcbbrrsrsChangeBatchRecords = _List1 # pChangeBatchRecords_
    }

-- | The next page marker.
lcbbrrsrsNextMarker :: Lens' ListChangeBatchesByRRSetResponse (Maybe Text)
lcbbrrsrsNextMarker = lens _lcbbrrsrsNextMarker (\ s a -> s{_lcbbrrsrsNextMarker = a});

-- | A flag that indicates if there are more change batches to list.
lcbbrrsrsIsTruncated :: Lens' ListChangeBatchesByRRSetResponse (Maybe Bool)
lcbbrrsrsIsTruncated = lens _lcbbrrsrsIsTruncated (\ s a -> s{_lcbbrrsrsIsTruncated = a});

-- | The response status code.
lcbbrrsrsResponseStatus :: Lens' ListChangeBatchesByRRSetResponse Int
lcbbrrsrsResponseStatus = lens _lcbbrrsrsResponseStatus (\ s a -> s{_lcbbrrsrsResponseStatus = a});

-- | The maximum number of items on a page.
lcbbrrsrsMaxItems :: Lens' ListChangeBatchesByRRSetResponse Text
lcbbrrsrsMaxItems = lens _lcbbrrsrsMaxItems (\ s a -> s{_lcbbrrsrsMaxItems = a});

-- | The page marker.
lcbbrrsrsMarker :: Lens' ListChangeBatchesByRRSetResponse Text
lcbbrrsrsMarker = lens _lcbbrrsrsMarker (\ s a -> s{_lcbbrrsrsMarker = a});

-- | The change batches within the given hosted zone and time period.
lcbbrrsrsChangeBatchRecords :: Lens' ListChangeBatchesByRRSetResponse (NonEmpty ChangeBatchRecord)
lcbbrrsrsChangeBatchRecords = lens _lcbbrrsrsChangeBatchRecords (\ s a -> s{_lcbbrrsrsChangeBatchRecords = a}) . _List1;
