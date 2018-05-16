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
-- Module      : Network.AWS.CloudWatchEvents.PutEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends custom events to Amazon CloudWatch Events so that they can be matched to rules.
--
--
module Network.AWS.CloudWatchEvents.PutEvents
    (
    -- * Creating a Request
      putEvents
    , PutEvents
    -- * Request Lenses
    , peEntries

    -- * Destructuring the Response
    , putEventsResponse
    , PutEventsResponse
    -- * Response Lenses
    , persFailedEntryCount
    , persEntries
    , persResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putEvents' smart constructor.
newtype PutEvents = PutEvents'
  { _peEntries :: List1 PutEventsRequestEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peEntries' - The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
putEvents
    :: NonEmpty PutEventsRequestEntry -- ^ 'peEntries'
    -> PutEvents
putEvents pEntries_ = PutEvents' {_peEntries = _List1 # pEntries_}


-- | The entry that defines an event in your system. You can specify several parameters for the entry such as the source and type of the event, resources associated with the event, and so on.
peEntries :: Lens' PutEvents (NonEmpty PutEventsRequestEntry)
peEntries = lens _peEntries (\ s a -> s{_peEntries = a}) . _List1

instance AWSRequest PutEvents where
        type Rs PutEvents = PutEventsResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 PutEventsResponse' <$>
                   (x .?> "FailedEntryCount") <*>
                     (x .?> "Entries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable PutEvents where

instance NFData PutEvents where

instance ToHeaders PutEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutEvents where
        toJSON PutEvents'{..}
          = object (catMaybes [Just ("Entries" .= _peEntries)])

instance ToPath PutEvents where
        toPath = const "/"

instance ToQuery PutEvents where
        toQuery = const mempty

-- | /See:/ 'putEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { _persFailedEntryCount :: !(Maybe Int)
  , _persEntries          :: !(Maybe [PutEventsResultEntry])
  , _persResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'persFailedEntryCount' - The number of failed entries.
--
-- * 'persEntries' - The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
--
-- * 'persResponseStatus' - -- | The response status code.
putEventsResponse
    :: Int -- ^ 'persResponseStatus'
    -> PutEventsResponse
putEventsResponse pResponseStatus_ =
  PutEventsResponse'
    { _persFailedEntryCount = Nothing
    , _persEntries = Nothing
    , _persResponseStatus = pResponseStatus_
    }


-- | The number of failed entries.
persFailedEntryCount :: Lens' PutEventsResponse (Maybe Int)
persFailedEntryCount = lens _persFailedEntryCount (\ s a -> s{_persFailedEntryCount = a})

-- | The successfully and unsuccessfully ingested events results. If the ingestion was successful, the entry has the event ID in it. Otherwise, you can use the error code and error message to identify the problem with the entry.
persEntries :: Lens' PutEventsResponse [PutEventsResultEntry]
persEntries = lens _persEntries (\ s a -> s{_persEntries = a}) . _Default . _Coerce

-- | -- | The response status code.
persResponseStatus :: Lens' PutEventsResponse Int
persResponseStatus = lens _persResponseStatus (\ s a -> s{_persResponseStatus = a})

instance NFData PutEventsResponse where
