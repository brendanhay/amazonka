{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.ListIdentities
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list containing all of the identities (email addresses and
-- domains) for a specific AWS Account, regardless of verification status.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListIdentities.html>
module Network.AWS.SES.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liIdentityType
    , liNextToken
    , liMaxItems

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirNextToken
    , lirIdentities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types

-- | /See:/ 'listIdentities' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'liIdentityType'
--
-- * 'liNextToken'
--
-- * 'liMaxItems'
data ListIdentities = ListIdentities'{_liIdentityType :: Maybe IdentityType, _liNextToken :: Maybe Text, _liMaxItems :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ListIdentities' smart constructor.
listIdentities :: ListIdentities
listIdentities = ListIdentities'{_liIdentityType = Nothing, _liNextToken = Nothing, _liMaxItems = Nothing};

-- | The type of the identities to list. Possible values are \"EmailAddress\"
-- and \"Domain\". If this parameter is omitted, then all identities will
-- be listed.
liIdentityType :: Lens' ListIdentities (Maybe IdentityType)
liIdentityType = lens _liIdentityType (\ s a -> s{_liIdentityType = a});

-- | The token to use for pagination.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a});

-- | The maximum number of identities per page. Possible values are 1-1000
-- inclusive.
liMaxItems :: Lens' ListIdentities (Maybe Int)
liMaxItems = lens _liMaxItems (\ s a -> s{_liMaxItems = a});

instance AWSRequest ListIdentities where
        type Sv ListIdentities = SES
        type Rs ListIdentities = ListIdentitiesResponse
        request = post
        response
          = receiveXMLWrapper "ListIdentitiesResult"
              (\ s h x ->
                 ListIdentitiesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Identities" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListIdentities where
        toHeaders = const mempty

instance ToPath ListIdentities where
        toPath = const "/"

instance ToQuery ListIdentities where
        toQuery ListIdentities'{..}
          = mconcat
              ["Action" =: ("ListIdentities" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "IdentityType" =: _liIdentityType,
               "NextToken" =: _liNextToken,
               "MaxItems" =: _liMaxItems]

-- | /See:/ 'listIdentitiesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirNextToken'
--
-- * 'lirIdentities'
data ListIdentitiesResponse = ListIdentitiesResponse'{_lirNextToken :: Maybe Text, _lirIdentities :: [Text]} deriving (Eq, Read, Show)

-- | 'ListIdentitiesResponse' smart constructor.
listIdentitiesResponse :: ListIdentitiesResponse
listIdentitiesResponse = ListIdentitiesResponse'{_lirNextToken = Nothing, _lirIdentities = mempty};

-- | The token used for pagination.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\ s a -> s{_lirNextToken = a});

-- | A list of identities.
lirIdentities :: Lens' ListIdentitiesResponse [Text]
lirIdentities = lens _lirIdentities (\ s a -> s{_lirIdentities = a});
