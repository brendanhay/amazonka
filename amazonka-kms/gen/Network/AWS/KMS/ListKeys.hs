{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.KMS.ListKeys
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

-- | Lists the customer master keys.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListKeys.html>
module Network.AWS.KMS.ListKeys
    (
    -- * Request
      ListKeys
    -- ** Request constructor
    , listKeys
    -- ** Request lenses
    , lkMarker
    , lkLimit

    -- * Response
    , ListKeysResponse
    -- ** Response constructor
    , listKeysResponse
    -- ** Response lenses
    , lkrTruncated
    , lkrKeys
    , lkrNextMarker
    , lkrStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listKeys' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkMarker'
--
-- * 'lkLimit'
data ListKeys = ListKeys'
    { _lkMarker :: !(Maybe Text)
    , _lkLimit  :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'ListKeys' smart constructor.
listKeys :: ListKeys
listKeys =
    ListKeys'
    { _lkMarker = Nothing
    , _lkLimit = Nothing
    }

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @NextMarker@ in the response
-- you just received.
lkMarker :: Lens' ListKeys (Maybe Text)
lkMarker = lens _lkMarker (\ s a -> s{_lkMarker = a});

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of keys you want listed in the response. If there are
-- additional keys beyond the maximum you specify, the @Truncated@ response
-- element will be set to @true.@
lkLimit :: Lens' ListKeys (Maybe Natural)
lkLimit = lens _lkLimit (\ s a -> s{_lkLimit = a}) . mapping _Nat;

instance AWSRequest ListKeys where
        type Sv ListKeys = KMS
        type Rs ListKeys = ListKeysResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListKeysResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "Keys" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListKeys where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListKeys" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListKeys where
        toJSON ListKeys'{..}
          = object ["Marker" .= _lkMarker, "Limit" .= _lkLimit]

instance ToPath ListKeys where
        toPath = const "/"

instance ToQuery ListKeys where
        toQuery = const mempty

-- | /See:/ 'listKeysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkrTruncated'
--
-- * 'lkrKeys'
--
-- * 'lkrNextMarker'
--
-- * 'lkrStatus'
data ListKeysResponse = ListKeysResponse'
    { _lkrTruncated  :: !(Maybe Bool)
    , _lkrKeys       :: !(Maybe [KeyListEntry])
    , _lkrNextMarker :: !(Maybe Text)
    , _lkrStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListKeysResponse' smart constructor.
listKeysResponse :: Int -> ListKeysResponse
listKeysResponse pStatus =
    ListKeysResponse'
    { _lkrTruncated = Nothing
    , _lkrKeys = Nothing
    , _lkrNextMarker = Nothing
    , _lkrStatus = pStatus
    }

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more keys in the list.
lkrTruncated :: Lens' ListKeysResponse (Maybe Bool)
lkrTruncated = lens _lkrTruncated (\ s a -> s{_lkrTruncated = a});

-- | A list of keys.
lkrKeys :: Lens' ListKeysResponse [KeyListEntry]
lkrKeys = lens _lkrKeys (\ s a -> s{_lkrKeys = a}) . _Default;

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
lkrNextMarker :: Lens' ListKeysResponse (Maybe Text)
lkrNextMarker = lens _lkrNextMarker (\ s a -> s{_lkrNextMarker = a});

-- | FIXME: Undocumented member.
lkrStatus :: Lens' ListKeysResponse Int
lkrStatus = lens _lkrStatus (\ s a -> s{_lkrStatus = a});
