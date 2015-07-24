{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListAliases
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the key aliases in the account.
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
    , larsTruncated
    , larsAliases
    , larsNextMarker
    , larsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
        request = postJSON "ListAliases"
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
-- * 'larsTruncated'
--
-- * 'larsAliases'
--
-- * 'larsNextMarker'
--
-- * 'larsStatus'
data ListAliasesResponse = ListAliasesResponse'
    { _larsTruncated  :: !(Maybe Bool)
    , _larsAliases    :: !(Maybe [AliasListEntry])
    , _larsNextMarker :: !(Maybe Text)
    , _larsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAliasesResponse' smart constructor.
listAliasesResponse :: Int -> ListAliasesResponse
listAliasesResponse pStatus_ =
    ListAliasesResponse'
    { _larsTruncated = Nothing
    , _larsAliases = Nothing
    , _larsNextMarker = Nothing
    , _larsStatus = pStatus_
    }

-- | A flag that indicates whether there are more items in the list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more aliases in the
-- list.
larsTruncated :: Lens' ListAliasesResponse (Maybe Bool)
larsTruncated = lens _larsTruncated (\ s a -> s{_larsTruncated = a});

-- | A list of key aliases in the user\'s account.
larsAliases :: Lens' ListAliasesResponse [AliasListEntry]
larsAliases = lens _larsAliases (\ s a -> s{_larsAliases = a}) . _Default;

-- | If @Truncated@ is true, this value is present and contains the value to
-- use for the @Marker@ request parameter in a subsequent pagination
-- request.
larsNextMarker :: Lens' ListAliasesResponse (Maybe Text)
larsNextMarker = lens _larsNextMarker (\ s a -> s{_larsNextMarker = a});

-- | FIXME: Undocumented member.
larsStatus :: Lens' ListAliasesResponse Int
larsStatus = lens _larsStatus (\ s a -> s{_larsStatus = a});
