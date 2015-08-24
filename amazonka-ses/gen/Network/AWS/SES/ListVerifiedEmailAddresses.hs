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
-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list containing all of the email addresses that have been
-- verified.
--
-- The ListVerifiedEmailAddresses action is deprecated as of the May 15,
-- 2012 release of Domain Verification. The ListIdentities action is now
-- preferred.
--
-- This action is throttled at one request per second.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListVerifiedEmailAddresses.html AWS API Reference> for ListVerifiedEmailAddresses.
module Network.AWS.SES.ListVerifiedEmailAddresses
    (
    -- * Creating a Request
      listVerifiedEmailAddresses
    , ListVerifiedEmailAddresses

    -- * Destructuring the Response
    , listVerifiedEmailAddressesResponse
    , ListVerifiedEmailAddressesResponse
    -- * Response Lenses
    , lvearsVerifiedEmailAddresses
    , lvearsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'listVerifiedEmailAddresses' smart constructor.
data ListVerifiedEmailAddresses =
    ListVerifiedEmailAddresses'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListVerifiedEmailAddresses' with the minimum fields required to make a request.
--
listVerifiedEmailAddresses
    :: ListVerifiedEmailAddresses
listVerifiedEmailAddresses = ListVerifiedEmailAddresses'

instance AWSRequest ListVerifiedEmailAddresses where
        type Rs ListVerifiedEmailAddresses =
             ListVerifiedEmailAddressesResponse
        request = postQuery sES
        response
          = receiveXMLWrapper
              "ListVerifiedEmailAddressesResult"
              (\ s h x ->
                 ListVerifiedEmailAddressesResponse' <$>
                   (x .@? "VerifiedEmailAddresses" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListVerifiedEmailAddresses where
        toHeaders = const mempty

instance ToPath ListVerifiedEmailAddresses where
        toPath = const "/"

instance ToQuery ListVerifiedEmailAddresses where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("ListVerifiedEmailAddresses" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | Represents a list of all the email addresses verified for the current
-- user.
--
-- /See:/ 'listVerifiedEmailAddressesResponse' smart constructor.
data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse'
    { _lvearsVerifiedEmailAddresses :: !(Maybe [Text])
    , _lvearsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListVerifiedEmailAddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvearsVerifiedEmailAddresses'
--
-- * 'lvearsStatus'
listVerifiedEmailAddressesResponse
    :: Int -- ^ 'lvearsStatus'
    -> ListVerifiedEmailAddressesResponse
listVerifiedEmailAddressesResponse pStatus_ =
    ListVerifiedEmailAddressesResponse'
    { _lvearsVerifiedEmailAddresses = Nothing
    , _lvearsStatus = pStatus_
    }

-- | A list of email addresses that have been verified.
lvearsVerifiedEmailAddresses :: Lens' ListVerifiedEmailAddressesResponse [Text]
lvearsVerifiedEmailAddresses = lens _lvearsVerifiedEmailAddresses (\ s a -> s{_lvearsVerifiedEmailAddresses = a}) . _Default . _Coerce;

-- | The response status code.
lvearsStatus :: Lens' ListVerifiedEmailAddressesResponse Int
lvearsStatus = lens _lvearsStatus (\ s a -> s{_lvearsStatus = a});
