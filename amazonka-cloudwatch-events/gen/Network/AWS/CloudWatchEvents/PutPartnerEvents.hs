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
-- Module      : Network.AWS.CloudWatchEvents.PutPartnerEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is used by SaaS partners to write events to a customer's partner event bus.
--
--
module Network.AWS.CloudWatchEvents.PutPartnerEvents
    (
    -- * Creating a Request
      putPartnerEvents
    , PutPartnerEvents
    -- * Request Lenses
    , ppeEntries

    -- * Destructuring the Response
    , putPartnerEventsResponse
    , PutPartnerEventsResponse
    -- * Response Lenses
    , ppersFailedEntryCount
    , ppersEntries
    , ppersResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putPartnerEvents' smart constructor.
newtype PutPartnerEvents = PutPartnerEvents'
  { _ppeEntries :: List1 PutPartnerEventsRequestEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPartnerEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppeEntries' - The list of events to write to the event bus.
putPartnerEvents
    :: NonEmpty PutPartnerEventsRequestEntry -- ^ 'ppeEntries'
    -> PutPartnerEvents
putPartnerEvents pEntries_ =
  PutPartnerEvents' {_ppeEntries = _List1 # pEntries_}


-- | The list of events to write to the event bus.
ppeEntries :: Lens' PutPartnerEvents (NonEmpty PutPartnerEventsRequestEntry)
ppeEntries = lens _ppeEntries (\ s a -> s{_ppeEntries = a}) . _List1

instance AWSRequest PutPartnerEvents where
        type Rs PutPartnerEvents = PutPartnerEventsResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 PutPartnerEventsResponse' <$>
                   (x .?> "FailedEntryCount") <*>
                     (x .?> "Entries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable PutPartnerEvents where

instance NFData PutPartnerEvents where

instance ToHeaders PutPartnerEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutPartnerEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutPartnerEvents where
        toJSON PutPartnerEvents'{..}
          = object
              (catMaybes [Just ("Entries" .= _ppeEntries)])

instance ToPath PutPartnerEvents where
        toPath = const "/"

instance ToQuery PutPartnerEvents where
        toQuery = const mempty

-- | /See:/ 'putPartnerEventsResponse' smart constructor.
data PutPartnerEventsResponse = PutPartnerEventsResponse'
  { _ppersFailedEntryCount :: !(Maybe Int)
  , _ppersEntries          :: !(Maybe [PutPartnerEventsResultEntry])
  , _ppersResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutPartnerEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppersFailedEntryCount' - The number of events from this operation that couldn't be written to the partner event bus.
--
-- * 'ppersEntries' - The list of events from this operation that were successfully written to the partner event bus.
--
-- * 'ppersResponseStatus' - -- | The response status code.
putPartnerEventsResponse
    :: Int -- ^ 'ppersResponseStatus'
    -> PutPartnerEventsResponse
putPartnerEventsResponse pResponseStatus_ =
  PutPartnerEventsResponse'
    { _ppersFailedEntryCount = Nothing
    , _ppersEntries = Nothing
    , _ppersResponseStatus = pResponseStatus_
    }


-- | The number of events from this operation that couldn't be written to the partner event bus.
ppersFailedEntryCount :: Lens' PutPartnerEventsResponse (Maybe Int)
ppersFailedEntryCount = lens _ppersFailedEntryCount (\ s a -> s{_ppersFailedEntryCount = a})

-- | The list of events from this operation that were successfully written to the partner event bus.
ppersEntries :: Lens' PutPartnerEventsResponse [PutPartnerEventsResultEntry]
ppersEntries = lens _ppersEntries (\ s a -> s{_ppersEntries = a}) . _Default . _Coerce

-- | -- | The response status code.
ppersResponseStatus :: Lens' PutPartnerEventsResponse Int
ppersResponseStatus = lens _ppersResponseStatus (\ s a -> s{_ppersResponseStatus = a})

instance NFData PutPartnerEventsResponse where
