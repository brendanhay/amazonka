{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListGrants
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- List the grants for a specified key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_ListGrants.html>
module Network.AWS.KMS.ListGrants
    (
    -- * Request
      ListGrants
    -- ** Request constructor
    , listGrants
    -- ** Request lenses
    , lgMarker
    , lgLimit
    , lgKeyId

    -- * Response
    , ListGrantsResponse
    -- ** Response constructor
    , listGrantsResponse
    -- ** Response lenses
    , lgrTruncated
    , lgrGrants
    , lgrNextMarker
    , lgrStatus
    ) where

import           Network.AWS.KMS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGrants' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgMarker'
--
-- * 'lgLimit'
--
-- * 'lgKeyId'
data ListGrants = ListGrants'
    { _lgMarker :: !(Maybe Text)
    , _lgLimit  :: !(Maybe Nat)
    , _lgKeyId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGrants' smart constructor.
listGrants :: Text -> ListGrants
listGrants pKeyId =
    ListGrants'
    { _lgMarker = Nothing
    , _lgLimit = Nothing
    , _lgKeyId = pKeyId
    }

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @NextMarker@ in the response
-- you just received.
lgMarker :: Lens' ListGrants (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a});

-- | Specify this parameter only when paginating results to indicate the
-- maximum number of grants you want listed in the response. If there are
-- additional grants beyond the maximum you specify, the @Truncated@
-- response element will be set to @true.@
lgLimit :: Lens' ListGrants (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat;

-- | A unique identifier for the customer master key. This value can be a
-- globally unique identifier or the fully specified ARN to a key.
--
-- -   Key ARN Example -
--     arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012
-- -   Globally Unique Key ID Example -
--     12345678-1234-1234-1234-123456789012
lgKeyId :: Lens' ListGrants Text
lgKeyId = lens _lgKeyId (\ s a -> s{_lgKeyId = a});

instance AWSRequest ListGrants where
        type Sv ListGrants = KMS
        type Rs ListGrants = ListGrantsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListGrantsResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "Grants" .!@ mempty)
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

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
              ["Marker" .= _lgMarker, "Limit" .= _lgLimit,
               "KeyId" .= _lgKeyId]

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
--
-- * 'lgrStatus'
data ListGrantsResponse = ListGrantsResponse'
    { _lgrTruncated  :: !(Maybe Bool)
    , _lgrGrants     :: !(Maybe [GrantListEntry])
    , _lgrNextMarker :: !(Maybe Text)
    , _lgrStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGrantsResponse' smart constructor.
listGrantsResponse :: Int -> ListGrantsResponse
listGrantsResponse pStatus =
    ListGrantsResponse'
    { _lgrTruncated = Nothing
    , _lgrGrants = Nothing
    , _lgrNextMarker = Nothing
    , _lgrStatus = pStatus
    }

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more grants in the
-- list.
lgrTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgrTruncated = lens _lgrTruncated (\ s a -> s{_lgrTruncated = a});

-- | A list of grants.
lgrGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgrGrants = lens _lgrGrants (\ s a -> s{_lgrGrants = a}) . _Default;

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
lgrNextMarker :: Lens' ListGrantsResponse (Maybe Text)
lgrNextMarker = lens _lgrNextMarker (\ s a -> s{_lgrNextMarker = a});

-- | FIXME: Undocumented member.
lgrStatus :: Lens' ListGrantsResponse Int
lgrStatus = lens _lgrStatus (\ s a -> s{_lgrStatus = a});
