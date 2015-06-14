{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.KMS.ListGrants
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

-- | List the grants for a specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListGrants.html>
module Network.AWS.KMS.ListGrants
    (
    -- * Request
      ListGrants
    -- ** Request constructor
    , listGrants
    -- ** Request lenses
    , lgKeyId
    , lgMarker
    , lgLimit

    -- * Response
    , ListGrantsResponse
    -- ** Response constructor
    , listGrantsResponse
    -- ** Response lenses
    , lgrTruncated
    , lgrGrants
    , lgrNextMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.KMS.Types

-- | /See:/ 'listGrants' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgKeyId'
--
-- * 'lgMarker'
--
-- * 'lgLimit'
data ListGrants = ListGrants'{_lgKeyId :: Text, _lgMarker :: Text, _lgLimit :: Nat} deriving (Eq, Read, Show)

-- | 'ListGrants' smart constructor.
listGrants :: Text -> Text -> Natural -> ListGrants
listGrants pKeyId pMarker pLimit = ListGrants'{_lgKeyId = pKeyId, _lgMarker = pMarker, _lgLimit = _Nat # pLimit};

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
lgKeyId :: Lens' ListGrants Text
lgKeyId = lens _lgKeyId (\ s a -> s{_lgKeyId = a});

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @NextMarker@ in the response
-- you just received.
lgMarker :: Lens' ListGrants Text
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of grants you want listed in the response. If there are
-- additional grants beyond the maximum you specify, the @Truncated@
-- response element will be set to @true.@
lgLimit :: Lens' ListGrants Natural
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . _Nat;

instance AWSRequest ListGrants where
        type Sv ListGrants = KMS
        type Rs ListGrants = ListGrantsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListGrantsResponse' <$>
                   x .?> "Truncated" <*> x .?> "Grants" .!@ mempty <*>
                     x .:> "NextMarker")

instance ToHeaders ListGrants where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListGrants" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGrants where
        toJSON ListGrants'{..}
          = object
              ["KeyId" .= _lgKeyId, "Marker" .= _lgMarker,
               "Limit" .= _lgLimit]

instance ToPath ListGrants where
        toPath = const "/"

instance ToQuery ListGrants where
        toQuery = const mempty

-- | /See:/ 'listGrantsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgrTruncated'
--
-- * 'lgrGrants'
--
-- * 'lgrNextMarker'
data ListGrantsResponse = ListGrantsResponse'{_lgrTruncated :: Maybe Bool, _lgrGrants :: [GrantListEntry], _lgrNextMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListGrantsResponse' smart constructor.
listGrantsResponse :: Text -> ListGrantsResponse
listGrantsResponse pNextMarker = ListGrantsResponse'{_lgrTruncated = Nothing, _lgrGrants = mempty, _lgrNextMarker = pNextMarker};

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more grants in the
-- list.
lgrTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgrTruncated = lens _lgrTruncated (\ s a -> s{_lgrTruncated = a});

-- | A list of grants.
lgrGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgrGrants = lens _lgrGrants (\ s a -> s{_lgrGrants = a});

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
lgrNextMarker :: Lens' ListGrantsResponse Text
lgrNextMarker = lens _lgrNextMarker (\ s a -> s{_lgrNextMarker = a});
