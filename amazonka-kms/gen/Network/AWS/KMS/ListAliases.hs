{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists all of the key aliases in the account.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListAliases.html>
module Network.AWS.KMS.ListAliases
    (
    -- * Request
      ListAliases
    -- ** Request constructor
    , listAliases
    -- ** Request lenses
    , laMarker
    , laLimit

    -- * Response
    , ListAliasesResponse
    -- ** Response constructor
    , listAliasesResponse
    -- ** Response lenses
    , larTruncated
    , larAliases
    , larNextMarker
    , larStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAliases' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laMarker'
--
-- * 'laLimit'
data ListAliases = ListAliases'
    { _laMarker :: !(Maybe Text)
    , _laLimit  :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'ListAliases' smart constructor.
listAliases :: ListAliases
listAliases =
    ListAliases'
    { _laMarker = Nothing
    , _laLimit = Nothing
    }

-- | Use this parameter when paginating results, and only in a subsequent
-- request after you\'ve received a response where the results are
-- truncated. Set it to the value of the @NextMarker@ element in the
-- response you just received.
laMarker :: Lens' ListAliases (Maybe Text)
laMarker = lens _laMarker (\ s a -> s{_laMarker = a});

-- | Specify this parameter when paginating results to indicate the maximum
-- number of aliases you want in each response. If there are additional
-- aliases beyond the maximum you specify, the @Truncated@ response element
-- will be set to @true.@
laLimit :: Lens' ListAliases (Maybe Natural)
laLimit = lens _laLimit (\ s a -> s{_laLimit = a}) . mapping _Nat;

instance AWSRequest ListAliases where
        type Sv ListAliases = KMS
        type Rs ListAliases = ListAliasesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListAliasesResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "Aliases" .!@ mempty)
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAliases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListAliases" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAliases where
        toJSON ListAliases'{..}
          = object ["Marker" .= _laMarker, "Limit" .= _laLimit]

instance ToPath ListAliases where
        toPath = const "/"

instance ToQuery ListAliases where
        toQuery = const mempty

-- | /See:/ 'listAliasesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larTruncated'
--
-- * 'larAliases'
--
-- * 'larNextMarker'
--
-- * 'larStatus'
data ListAliasesResponse = ListAliasesResponse'
    { _larTruncated  :: !(Maybe Bool)
    , _larAliases    :: !(Maybe [AliasListEntry])
    , _larNextMarker :: !(Maybe Text)
    , _larStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListAliasesResponse' smart constructor.
listAliasesResponse :: Int -> ListAliasesResponse
listAliasesResponse pStatus =
    ListAliasesResponse'
    { _larTruncated = Nothing
    , _larAliases = Nothing
    , _larNextMarker = Nothing
    , _larStatus = pStatus
    }

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more aliases in the
-- list.
larTruncated :: Lens' ListAliasesResponse (Maybe Bool)
larTruncated = lens _larTruncated (\ s a -> s{_larTruncated = a});

-- | A list of key aliases in the user\'s account.
larAliases :: Lens' ListAliasesResponse [AliasListEntry]
larAliases = lens _larAliases (\ s a -> s{_larAliases = a}) . _Default;

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
larNextMarker :: Lens' ListAliasesResponse (Maybe Text)
larNextMarker = lens _larNextMarker (\ s a -> s{_larNextMarker = a});

-- | FIXME: Undocumented member.
larStatus :: Lens' ListAliasesResponse Int
larStatus = lens _larStatus (\ s a -> s{_larStatus = a});
