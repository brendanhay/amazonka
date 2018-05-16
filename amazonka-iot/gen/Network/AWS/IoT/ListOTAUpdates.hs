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
-- Module      : Network.AWS.IoT.ListOTAUpdates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists OTA updates.
--
--
module Network.AWS.IoT.ListOTAUpdates
    (
    -- * Creating a Request
      listOTAUpdates
    , ListOTAUpdates
    -- * Request Lenses
    , lotauNextToken
    , lotauOtaUpdateStatus
    , lotauMaxResults

    -- * Destructuring the Response
    , listOTAUpdatesResponse
    , ListOTAUpdatesResponse
    -- * Response Lenses
    , lotaursNextToken
    , lotaursOtaUpdates
    , lotaursResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { _lotauNextToken       :: !(Maybe Text)
  , _lotauOtaUpdateStatus :: !(Maybe OTAUpdateStatus)
  , _lotauMaxResults      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOTAUpdates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lotauNextToken' - A token used to retrieve the next set of results.
--
-- * 'lotauOtaUpdateStatus' - The OTA update job status.
--
-- * 'lotauMaxResults' - The maximum number of results to return at one time.
listOTAUpdates
    :: ListOTAUpdates
listOTAUpdates =
  ListOTAUpdates'
    { _lotauNextToken = Nothing
    , _lotauOtaUpdateStatus = Nothing
    , _lotauMaxResults = Nothing
    }


-- | A token used to retrieve the next set of results.
lotauNextToken :: Lens' ListOTAUpdates (Maybe Text)
lotauNextToken = lens _lotauNextToken (\ s a -> s{_lotauNextToken = a})

-- | The OTA update job status.
lotauOtaUpdateStatus :: Lens' ListOTAUpdates (Maybe OTAUpdateStatus)
lotauOtaUpdateStatus = lens _lotauOtaUpdateStatus (\ s a -> s{_lotauOtaUpdateStatus = a})

-- | The maximum number of results to return at one time.
lotauMaxResults :: Lens' ListOTAUpdates (Maybe Natural)
lotauMaxResults = lens _lotauMaxResults (\ s a -> s{_lotauMaxResults = a}) . mapping _Nat

instance AWSRequest ListOTAUpdates where
        type Rs ListOTAUpdates = ListOTAUpdatesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListOTAUpdatesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "otaUpdates" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOTAUpdates where

instance NFData ListOTAUpdates where

instance ToHeaders ListOTAUpdates where
        toHeaders = const mempty

instance ToPath ListOTAUpdates where
        toPath = const "/otaUpdates"

instance ToQuery ListOTAUpdates where
        toQuery ListOTAUpdates'{..}
          = mconcat
              ["nextToken" =: _lotauNextToken,
               "otaUpdateStatus" =: _lotauOtaUpdateStatus,
               "maxResults" =: _lotauMaxResults]

-- | /See:/ 'listOTAUpdatesResponse' smart constructor.
data ListOTAUpdatesResponse = ListOTAUpdatesResponse'
  { _lotaursNextToken      :: !(Maybe Text)
  , _lotaursOtaUpdates     :: !(Maybe [OTAUpdateSummary])
  , _lotaursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOTAUpdatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lotaursNextToken' - A token to use to get the next set of results.
--
-- * 'lotaursOtaUpdates' - A list of OTA update jobs.
--
-- * 'lotaursResponseStatus' - -- | The response status code.
listOTAUpdatesResponse
    :: Int -- ^ 'lotaursResponseStatus'
    -> ListOTAUpdatesResponse
listOTAUpdatesResponse pResponseStatus_ =
  ListOTAUpdatesResponse'
    { _lotaursNextToken = Nothing
    , _lotaursOtaUpdates = Nothing
    , _lotaursResponseStatus = pResponseStatus_
    }


-- | A token to use to get the next set of results.
lotaursNextToken :: Lens' ListOTAUpdatesResponse (Maybe Text)
lotaursNextToken = lens _lotaursNextToken (\ s a -> s{_lotaursNextToken = a})

-- | A list of OTA update jobs.
lotaursOtaUpdates :: Lens' ListOTAUpdatesResponse [OTAUpdateSummary]
lotaursOtaUpdates = lens _lotaursOtaUpdates (\ s a -> s{_lotaursOtaUpdates = a}) . _Default . _Coerce

-- | -- | The response status code.
lotaursResponseStatus :: Lens' ListOTAUpdatesResponse Int
lotaursResponseStatus = lens _lotaursResponseStatus (\ s a -> s{_lotaursResponseStatus = a})

instance NFData ListOTAUpdatesResponse where
