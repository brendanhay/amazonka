{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListVerifiedEmailAddresses.html>
module Network.AWS.SES.ListVerifiedEmailAddresses
    (
    -- * Request
      ListVerifiedEmailAddresses
    -- ** Request constructor
    , listVerifiedEmailAddresses

    -- * Response
    , ListVerifiedEmailAddressesResponse
    -- ** Response constructor
    , listVerifiedEmailAddressesResponse
    -- ** Response lenses
    , lvearsVerifiedEmailAddresses
    , lvearsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | /See:/ 'listVerifiedEmailAddresses' smart constructor.
data ListVerifiedEmailAddresses =
    ListVerifiedEmailAddresses'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVerifiedEmailAddresses' smart constructor.
listVerifiedEmailAddresses :: ListVerifiedEmailAddresses
listVerifiedEmailAddresses = ListVerifiedEmailAddresses'

instance AWSRequest ListVerifiedEmailAddresses where
        type Sv ListVerifiedEmailAddresses = SES
        type Rs ListVerifiedEmailAddresses =
             ListVerifiedEmailAddressesResponse
        request = post
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvearsVerifiedEmailAddresses'
--
-- * 'lvearsStatus'
data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse'
    { _lvearsVerifiedEmailAddresses :: !(Maybe [Text])
    , _lvearsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVerifiedEmailAddressesResponse' smart constructor.
listVerifiedEmailAddressesResponse :: Int -> ListVerifiedEmailAddressesResponse
listVerifiedEmailAddressesResponse pStatus_ =
    ListVerifiedEmailAddressesResponse'
    { _lvearsVerifiedEmailAddresses = Nothing
    , _lvearsStatus = pStatus_
    }

-- | A list of email addresses that have been verified.
lvearsVerifiedEmailAddresses :: Lens' ListVerifiedEmailAddressesResponse [Text]
lvearsVerifiedEmailAddresses = lens _lvearsVerifiedEmailAddresses (\ s a -> s{_lvearsVerifiedEmailAddresses = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
lvearsStatus :: Lens' ListVerifiedEmailAddressesResponse Int
lvearsStatus = lens _lvearsStatus (\ s a -> s{_lvearsStatus = a});
