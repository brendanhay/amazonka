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
-- Module      : Network.AWS.AlexaBusiness.UpdateConferenceProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing conference provider's settings.
--
--
module Network.AWS.AlexaBusiness.UpdateConferenceProvider
    (
    -- * Creating a Request
      updateConferenceProvider
    , UpdateConferenceProvider
    -- * Request Lenses
    , ucpPSTNDialIn
    , ucpIPDialIn
    , ucpConferenceProviderARN
    , ucpConferenceProviderType
    , ucpMeetingSetting

    -- * Destructuring the Response
    , updateConferenceProviderResponse
    , UpdateConferenceProviderResponse
    -- * Response Lenses
    , ucprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateConferenceProvider' smart constructor.
data UpdateConferenceProvider = UpdateConferenceProvider'
  { _ucpPSTNDialIn             :: !(Maybe PSTNDialIn)
  , _ucpIPDialIn               :: !(Maybe IPDialIn)
  , _ucpConferenceProviderARN  :: !Text
  , _ucpConferenceProviderType :: !ConferenceProviderType
  , _ucpMeetingSetting         :: !MeetingSetting
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConferenceProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucpPSTNDialIn' - The information for PSTN conferencing.
--
-- * 'ucpIPDialIn' - The IP endpoint and protocol for calling.
--
-- * 'ucpConferenceProviderARN' - The ARN of the conference provider.
--
-- * 'ucpConferenceProviderType' - The type of the conference provider.
--
-- * 'ucpMeetingSetting' - The meeting settings for the conference provider.
updateConferenceProvider
    :: Text -- ^ 'ucpConferenceProviderARN'
    -> ConferenceProviderType -- ^ 'ucpConferenceProviderType'
    -> MeetingSetting -- ^ 'ucpMeetingSetting'
    -> UpdateConferenceProvider
updateConferenceProvider pConferenceProviderARN_ pConferenceProviderType_ pMeetingSetting_ =
  UpdateConferenceProvider'
    { _ucpPSTNDialIn = Nothing
    , _ucpIPDialIn = Nothing
    , _ucpConferenceProviderARN = pConferenceProviderARN_
    , _ucpConferenceProviderType = pConferenceProviderType_
    , _ucpMeetingSetting = pMeetingSetting_
    }


-- | The information for PSTN conferencing.
ucpPSTNDialIn :: Lens' UpdateConferenceProvider (Maybe PSTNDialIn)
ucpPSTNDialIn = lens _ucpPSTNDialIn (\ s a -> s{_ucpPSTNDialIn = a})

-- | The IP endpoint and protocol for calling.
ucpIPDialIn :: Lens' UpdateConferenceProvider (Maybe IPDialIn)
ucpIPDialIn = lens _ucpIPDialIn (\ s a -> s{_ucpIPDialIn = a})

-- | The ARN of the conference provider.
ucpConferenceProviderARN :: Lens' UpdateConferenceProvider Text
ucpConferenceProviderARN = lens _ucpConferenceProviderARN (\ s a -> s{_ucpConferenceProviderARN = a})

-- | The type of the conference provider.
ucpConferenceProviderType :: Lens' UpdateConferenceProvider ConferenceProviderType
ucpConferenceProviderType = lens _ucpConferenceProviderType (\ s a -> s{_ucpConferenceProviderType = a})

-- | The meeting settings for the conference provider.
ucpMeetingSetting :: Lens' UpdateConferenceProvider MeetingSetting
ucpMeetingSetting = lens _ucpMeetingSetting (\ s a -> s{_ucpMeetingSetting = a})

instance AWSRequest UpdateConferenceProvider where
        type Rs UpdateConferenceProvider =
             UpdateConferenceProviderResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateConferenceProviderResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateConferenceProvider where

instance NFData UpdateConferenceProvider where

instance ToHeaders UpdateConferenceProvider where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.UpdateConferenceProvider" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateConferenceProvider where
        toJSON UpdateConferenceProvider'{..}
          = object
              (catMaybes
                 [("PSTNDialIn" .=) <$> _ucpPSTNDialIn,
                  ("IPDialIn" .=) <$> _ucpIPDialIn,
                  Just
                    ("ConferenceProviderArn" .=
                       _ucpConferenceProviderARN),
                  Just
                    ("ConferenceProviderType" .=
                       _ucpConferenceProviderType),
                  Just ("MeetingSetting" .= _ucpMeetingSetting)])

instance ToPath UpdateConferenceProvider where
        toPath = const "/"

instance ToQuery UpdateConferenceProvider where
        toQuery = const mempty

-- | /See:/ 'updateConferenceProviderResponse' smart constructor.
newtype UpdateConferenceProviderResponse = UpdateConferenceProviderResponse'
  { _ucprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConferenceProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucprsResponseStatus' - -- | The response status code.
updateConferenceProviderResponse
    :: Int -- ^ 'ucprsResponseStatus'
    -> UpdateConferenceProviderResponse
updateConferenceProviderResponse pResponseStatus_ =
  UpdateConferenceProviderResponse' {_ucprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucprsResponseStatus :: Lens' UpdateConferenceProviderResponse Int
ucprsResponseStatus = lens _ucprsResponseStatus (\ s a -> s{_ucprsResponseStatus = a})

instance NFData UpdateConferenceProviderResponse
         where
