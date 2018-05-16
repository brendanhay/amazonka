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
-- Module      : Network.AWS.MechanicalTurk.UpdateExpirationForHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateExpirationForHIT@ operation allows you update the expiration time of a HIT. If you update it to a time in the past, the HIT will be immediately expired.
--
--
module Network.AWS.MechanicalTurk.UpdateExpirationForHIT
    (
    -- * Creating a Request
      updateExpirationForHIT
    , UpdateExpirationForHIT
    -- * Request Lenses
    , uefhitHITId
    , uefhitExpireAt

    -- * Destructuring the Response
    , updateExpirationForHITResponse
    , UpdateExpirationForHITResponse
    -- * Response Lenses
    , uefhitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { _uefhitHITId    :: !Text
  , _uefhitExpireAt :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateExpirationForHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uefhitHITId' - The HIT to update.
--
-- * 'uefhitExpireAt' - The date and time at which you want the HIT to expire
updateExpirationForHIT
    :: Text -- ^ 'uefhitHITId'
    -> UTCTime -- ^ 'uefhitExpireAt'
    -> UpdateExpirationForHIT
updateExpirationForHIT pHITId_ pExpireAt_ =
  UpdateExpirationForHIT'
    {_uefhitHITId = pHITId_, _uefhitExpireAt = _Time # pExpireAt_}


-- | The HIT to update.
uefhitHITId :: Lens' UpdateExpirationForHIT Text
uefhitHITId = lens _uefhitHITId (\ s a -> s{_uefhitHITId = a})

-- | The date and time at which you want the HIT to expire
uefhitExpireAt :: Lens' UpdateExpirationForHIT UTCTime
uefhitExpireAt = lens _uefhitExpireAt (\ s a -> s{_uefhitExpireAt = a}) . _Time

instance AWSRequest UpdateExpirationForHIT where
        type Rs UpdateExpirationForHIT =
             UpdateExpirationForHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateExpirationForHITResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateExpirationForHIT where

instance NFData UpdateExpirationForHIT where

instance ToHeaders UpdateExpirationForHIT where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.UpdateExpirationForHIT"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateExpirationForHIT where
        toJSON UpdateExpirationForHIT'{..}
          = object
              (catMaybes
                 [Just ("HITId" .= _uefhitHITId),
                  Just ("ExpireAt" .= _uefhitExpireAt)])

instance ToPath UpdateExpirationForHIT where
        toPath = const "/"

instance ToQuery UpdateExpirationForHIT where
        toQuery = const mempty

-- | /See:/ 'updateExpirationForHITResponse' smart constructor.
newtype UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
  { _uefhitrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateExpirationForHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uefhitrsResponseStatus' - -- | The response status code.
updateExpirationForHITResponse
    :: Int -- ^ 'uefhitrsResponseStatus'
    -> UpdateExpirationForHITResponse
updateExpirationForHITResponse pResponseStatus_ =
  UpdateExpirationForHITResponse' {_uefhitrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uefhitrsResponseStatus :: Lens' UpdateExpirationForHITResponse Int
uefhitrsResponseStatus = lens _uefhitrsResponseStatus (\ s a -> s{_uefhitrsResponseStatus = a})

instance NFData UpdateExpirationForHITResponse where
