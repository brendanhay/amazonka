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
-- Module      : Network.AWS.AlexaBusiness.PutConferencePreference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the conference preferences on a specific conference provider at the account level.
--
--
module Network.AWS.AlexaBusiness.PutConferencePreference
    (
    -- * Creating a Request
      putConferencePreference
    , PutConferencePreference
    -- * Request Lenses
    , pcpConferencePreference

    -- * Destructuring the Response
    , putConferencePreferenceResponse
    , PutConferencePreferenceResponse
    -- * Response Lenses
    , pcprsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putConferencePreference' smart constructor.
newtype PutConferencePreference = PutConferencePreference'
  { _pcpConferencePreference :: ConferencePreference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutConferencePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcpConferencePreference' - The conference preference of a specific conference provider.
putConferencePreference
    :: ConferencePreference -- ^ 'pcpConferencePreference'
    -> PutConferencePreference
putConferencePreference pConferencePreference_ =
  PutConferencePreference' {_pcpConferencePreference = pConferencePreference_}


-- | The conference preference of a specific conference provider.
pcpConferencePreference :: Lens' PutConferencePreference ConferencePreference
pcpConferencePreference = lens _pcpConferencePreference (\ s a -> s{_pcpConferencePreference = a})

instance AWSRequest PutConferencePreference where
        type Rs PutConferencePreference =
             PutConferencePreferenceResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 PutConferencePreferenceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutConferencePreference where

instance NFData PutConferencePreference where

instance ToHeaders PutConferencePreference where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.PutConferencePreference" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutConferencePreference where
        toJSON PutConferencePreference'{..}
          = object
              (catMaybes
                 [Just
                    ("ConferencePreference" .=
                       _pcpConferencePreference)])

instance ToPath PutConferencePreference where
        toPath = const "/"

instance ToQuery PutConferencePreference where
        toQuery = const mempty

-- | /See:/ 'putConferencePreferenceResponse' smart constructor.
newtype PutConferencePreferenceResponse = PutConferencePreferenceResponse'
  { _pcprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutConferencePreferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcprsResponseStatus' - -- | The response status code.
putConferencePreferenceResponse
    :: Int -- ^ 'pcprsResponseStatus'
    -> PutConferencePreferenceResponse
putConferencePreferenceResponse pResponseStatus_ =
  PutConferencePreferenceResponse' {_pcprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
pcprsResponseStatus :: Lens' PutConferencePreferenceResponse Int
pcprsResponseStatus = lens _pcprsResponseStatus (\ s a -> s{_pcprsResponseStatus = a})

instance NFData PutConferencePreferenceResponse where
