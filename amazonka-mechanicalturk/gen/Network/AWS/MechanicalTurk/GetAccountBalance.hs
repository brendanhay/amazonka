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
-- Module      : Network.AWS.MechanicalTurk.GetAccountBalance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAccountBalance@ operation retrieves the amount of money in your Amazon Mechanical Turk account.
--
--
module Network.AWS.MechanicalTurk.GetAccountBalance
    (
    -- * Creating a Request
      getAccountBalance
    , GetAccountBalance

    -- * Destructuring the Response
    , getAccountBalanceResponse
    , GetAccountBalanceResponse
    -- * Response Lenses
    , gabrsAvailableBalance
    , gabrsOnHoldBalance
    , gabrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccountBalance' smart constructor.
data GetAccountBalance =
  GetAccountBalance'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountBalance' with the minimum fields required to make a request.
--
getAccountBalance
    :: GetAccountBalance
getAccountBalance = GetAccountBalance'


instance AWSRequest GetAccountBalance where
        type Rs GetAccountBalance = GetAccountBalanceResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 GetAccountBalanceResponse' <$>
                   (x .?> "AvailableBalance") <*>
                     (x .?> "OnHoldBalance")
                     <*> (pure (fromEnum s)))

instance Hashable GetAccountBalance where

instance NFData GetAccountBalance where

instance ToHeaders GetAccountBalance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.GetAccountBalance"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAccountBalance where
        toJSON = const (Object mempty)

instance ToPath GetAccountBalance where
        toPath = const "/"

instance ToQuery GetAccountBalance where
        toQuery = const mempty

-- | /See:/ 'getAccountBalanceResponse' smart constructor.
data GetAccountBalanceResponse = GetAccountBalanceResponse'
  { _gabrsAvailableBalance :: !(Maybe Text)
  , _gabrsOnHoldBalance    :: !(Maybe Text)
  , _gabrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountBalanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gabrsAvailableBalance' - Undocumented member.
--
-- * 'gabrsOnHoldBalance' - Undocumented member.
--
-- * 'gabrsResponseStatus' - -- | The response status code.
getAccountBalanceResponse
    :: Int -- ^ 'gabrsResponseStatus'
    -> GetAccountBalanceResponse
getAccountBalanceResponse pResponseStatus_ =
  GetAccountBalanceResponse'
    { _gabrsAvailableBalance = Nothing
    , _gabrsOnHoldBalance = Nothing
    , _gabrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gabrsAvailableBalance :: Lens' GetAccountBalanceResponse (Maybe Text)
gabrsAvailableBalance = lens _gabrsAvailableBalance (\ s a -> s{_gabrsAvailableBalance = a})

-- | Undocumented member.
gabrsOnHoldBalance :: Lens' GetAccountBalanceResponse (Maybe Text)
gabrsOnHoldBalance = lens _gabrsOnHoldBalance (\ s a -> s{_gabrsOnHoldBalance = a})

-- | -- | The response status code.
gabrsResponseStatus :: Lens' GetAccountBalanceResponse Int
gabrsResponseStatus = lens _gabrsResponseStatus (\ s a -> s{_gabrsResponseStatus = a})

instance NFData GetAccountBalanceResponse where
