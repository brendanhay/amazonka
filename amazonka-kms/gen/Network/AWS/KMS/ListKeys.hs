{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListKeys
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the customer master keys.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListKeys.html>
module Network.AWS.KMS.ListKeys
    (
    -- * Request
      ListKeys
    -- ** Request constructor
    , listKeys
    -- ** Request lenses
    , lkrqMarker
    , lkrqLimit

    -- * Response
    , ListKeysResponse
    -- ** Response constructor
    , listKeysResponse
    -- ** Response lenses
    , lkrsTruncated
    , lkrsKeys
    , lkrsNextMarker
    , lkrsStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listKeys' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkrqMarker'
--
-- * 'lkrqLimit'
data ListKeys = ListKeys'
    { _lkrqMarker :: !(Maybe Text)
    , _lkrqLimit  :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListKeys' smart constructor.
listKeys :: ListKeys
listKeys =
    ListKeys'
    { _lkrqMarker = Nothing
    , _lkrqLimit = Nothing
    }

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @NextMarker@ in the response
-- you just received.
lkrqMarker :: Lens' ListKeys (Maybe Text)
lkrqMarker = lens _lkrqMarker (\ s a -> s{_lkrqMarker = a});

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of keys you want listed in the response. If there are
-- additional keys beyond the maximum you specify, the @Truncated@ response
-- element will be set to @true.@
lkrqLimit :: Lens' ListKeys (Maybe Natural)
lkrqLimit = lens _lkrqLimit (\ s a -> s{_lkrqLimit = a}) . mapping _Nat;

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
          = object
              ["Marker" .= _lkrqMarker, "Limit" .= _lkrqLimit]

instance ToPath ListKeys where
        toPath = const "/"

instance ToQuery ListKeys where
        toQuery = const mempty

-- | /See:/ 'listKeysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lkrsTruncated'
--
-- * 'lkrsKeys'
--
-- * 'lkrsNextMarker'
--
-- * 'lkrsStatus'
data ListKeysResponse = ListKeysResponse'
    { _lkrsTruncated  :: !(Maybe Bool)
    , _lkrsKeys       :: !(Maybe [KeyListEntry])
    , _lkrsNextMarker :: !(Maybe Text)
    , _lkrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListKeysResponse' smart constructor.
listKeysResponse :: Int -> ListKeysResponse
listKeysResponse pStatus =
    ListKeysResponse'
    { _lkrsTruncated = Nothing
    , _lkrsKeys = Nothing
    , _lkrsNextMarker = Nothing
    , _lkrsStatus = pStatus
    }

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more keys in the list.
lkrsTruncated :: Lens' ListKeysResponse (Maybe Bool)
lkrsTruncated = lens _lkrsTruncated (\ s a -> s{_lkrsTruncated = a});

-- | A list of keys.
lkrsKeys :: Lens' ListKeysResponse [KeyListEntry]
lkrsKeys = lens _lkrsKeys (\ s a -> s{_lkrsKeys = a}) . _Default;

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
lkrsNextMarker :: Lens' ListKeysResponse (Maybe Text)
lkrsNextMarker = lens _lkrsNextMarker (\ s a -> s{_lkrsNextMarker = a});

-- | FIXME: Undocumented member.
lkrsStatus :: Lens' ListKeysResponse Int
lkrsStatus = lens _lkrsStatus (\ s a -> s{_lkrsStatus = a});
