{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListAccountAliases
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

-- | Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccountAliases.html>
module Network.AWS.IAM.ListAccountAliases
    (
    -- * Request
      ListAccountAliases
    -- ** Request constructor
    , listAccountAliases
    -- ** Request lenses
    , laaMaxItems
    , laaMarker

    -- * Response
    , ListAccountAliasesResponse
    -- ** Response constructor
    , listAccountAliasesResponse
    -- ** Response lenses
    , laarIsTruncated
    , laarAccountAliases
    , laarMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listAccountAliases' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laaMaxItems'
--
-- * 'laaMarker'
data ListAccountAliases = ListAccountAliases'{_laaMaxItems :: Nat, _laaMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListAccountAliases' smart constructor.
listAccountAliases :: Natural -> Text -> ListAccountAliases
listAccountAliases pMaxItems pMarker = ListAccountAliases'{_laaMaxItems = _Nat # pMaxItems, _laaMarker = pMarker};

-- | Use this only when paginating results to indicate the maximum number of
-- account aliases you want in the response. If there are additional
-- account aliases beyond the maximum you specify, the @IsTruncated@
-- response element is @true@. This parameter is optional. If you do not
-- include it, it defaults to 100.
laaMaxItems :: Lens' ListAccountAliases Natural
laaMaxItems = lens _laaMaxItems (\ s a -> s{_laaMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
laaMarker :: Lens' ListAccountAliases Text
laaMarker = lens _laaMarker (\ s a -> s{_laaMarker = a});

instance AWSRequest ListAccountAliases where
        type Sv ListAccountAliases = IAM
        type Rs ListAccountAliases =
             ListAccountAliasesResponse
        request = post
        response
          = receiveXMLWrapper "ListAccountAliasesResult"
              (\ s h x ->
                 ListAccountAliasesResponse' <$>
                   x .@? "IsTruncated" <*>
                     (x .@? "AccountAliases" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders ListAccountAliases where
        toHeaders = const mempty

instance ToPath ListAccountAliases where
        toPath = const "/"

instance ToQuery ListAccountAliases where
        toQuery ListAccountAliases'{..}
          = mconcat
              ["Action" =: ("ListAccountAliases" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _laaMaxItems, "Marker" =: _laaMarker]

-- | /See:/ 'listAccountAliasesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laarIsTruncated'
--
-- * 'laarAccountAliases'
--
-- * 'laarMarker'
data ListAccountAliasesResponse = ListAccountAliasesResponse'{_laarIsTruncated :: Maybe Bool, _laarAccountAliases :: [Text], _laarMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListAccountAliasesResponse' smart constructor.
listAccountAliasesResponse :: [Text] -> Text -> ListAccountAliasesResponse
listAccountAliasesResponse pAccountAliases pMarker = ListAccountAliasesResponse'{_laarIsTruncated = Nothing, _laarAccountAliases = pAccountAliases, _laarMarker = pMarker};

-- | A flag that indicates whether there are more account aliases to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more account
-- aliases in the list.
laarIsTruncated :: Lens' ListAccountAliasesResponse (Maybe Bool)
laarIsTruncated = lens _laarIsTruncated (\ s a -> s{_laarIsTruncated = a});

-- | A list of aliases associated with the account.
laarAccountAliases :: Lens' ListAccountAliasesResponse [Text]
laarAccountAliases = lens _laarAccountAliases (\ s a -> s{_laarAccountAliases = a});

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
laarMarker :: Lens' ListAccountAliasesResponse Text
laarMarker = lens _laarMarker (\ s a -> s{_laarMarker = a});
