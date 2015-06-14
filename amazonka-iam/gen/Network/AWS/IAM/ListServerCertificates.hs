{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListServerCertificates
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

-- | Lists the server certificates that have the specified path prefix. If
-- none exist, the action returns an empty list.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListServerCertificates.html>
module Network.AWS.IAM.ListServerCertificates
    (
    -- * Request
      ListServerCertificates
    -- ** Request constructor
    , listServerCertificates
    -- ** Request lenses
    , lscPathPrefix
    , lscMaxItems
    , lscMarker

    -- * Response
    , ListServerCertificatesResponse
    -- ** Response constructor
    , listServerCertificatesResponse
    -- ** Response lenses
    , lscrMarker
    , lscrIsTruncated
    , lscrServerCertificateMetadataList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listServerCertificates' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscPathPrefix'
--
-- * 'lscMaxItems'
--
-- * 'lscMarker'
data ListServerCertificates = ListServerCertificates'{_lscPathPrefix :: Maybe Text, _lscMaxItems :: Maybe Nat, _lscMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListServerCertificates' smart constructor.
listServerCertificates :: ListServerCertificates
listServerCertificates = ListServerCertificates'{_lscPathPrefix = Nothing, _lscMaxItems = Nothing, _lscMarker = Nothing};

-- | The path prefix for filtering the results. For example:
-- @\/company\/servercerts@ would get all server certificates for which the
-- path starts with @\/company\/servercerts@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all server certificates.
lscPathPrefix :: Lens' ListServerCertificates (Maybe Text)
lscPathPrefix = lens _lscPathPrefix (\ s a -> s{_lscPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- server certificates you want in the response. If there are additional
-- server certificates beyond the maximum you specify, the @IsTruncated@
-- response element will be set to @true@. This parameter is optional. If
-- you do not include it, it defaults to 100.
lscMaxItems :: Lens' ListServerCertificates (Maybe Natural)
lscMaxItems = lens _lscMaxItems (\ s a -> s{_lscMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lscMarker :: Lens' ListServerCertificates (Maybe Text)
lscMarker = lens _lscMarker (\ s a -> s{_lscMarker = a});

instance AWSRequest ListServerCertificates where
        type Sv ListServerCertificates = IAM
        type Rs ListServerCertificates =
             ListServerCertificatesResponse
        request = post
        response
          = receiveXMLWrapper "ListServerCertificatesResult"
              (\ s h x ->
                 ListServerCertificatesResponse' <$>
                   x .@? "Marker" <*> x .@? "IsTruncated" <*>
                     (x .@? "ServerCertificateMetadataList" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListServerCertificates where
        toHeaders = const mempty

instance ToPath ListServerCertificates where
        toPath = const "/"

instance ToQuery ListServerCertificates where
        toQuery ListServerCertificates'{..}
          = mconcat
              ["Action" =:
                 ("ListServerCertificates" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lscPathPrefix,
               "MaxItems" =: _lscMaxItems, "Marker" =: _lscMarker]

-- | /See:/ 'listServerCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscrMarker'
--
-- * 'lscrIsTruncated'
--
-- * 'lscrServerCertificateMetadataList'
data ListServerCertificatesResponse = ListServerCertificatesResponse'{_lscrMarker :: Maybe Text, _lscrIsTruncated :: Maybe Bool, _lscrServerCertificateMetadataList :: [ServerCertificateMetadata]} deriving (Eq, Read, Show)

-- | 'ListServerCertificatesResponse' smart constructor.
listServerCertificatesResponse :: ListServerCertificatesResponse
listServerCertificatesResponse = ListServerCertificatesResponse'{_lscrMarker = Nothing, _lscrIsTruncated = Nothing, _lscrServerCertificateMetadataList = mempty};

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lscrMarker :: Lens' ListServerCertificatesResponse (Maybe Text)
lscrMarker = lens _lscrMarker (\ s a -> s{_lscrMarker = a});

-- | A flag that indicates whether there are more server certificates to
-- list. If your results were truncated, you can make a subsequent
-- pagination request using the @Marker@ request parameter to retrieve more
-- server certificates in the list.
lscrIsTruncated :: Lens' ListServerCertificatesResponse (Maybe Bool)
lscrIsTruncated = lens _lscrIsTruncated (\ s a -> s{_lscrIsTruncated = a});

-- | A list of server certificates.
lscrServerCertificateMetadataList :: Lens' ListServerCertificatesResponse [ServerCertificateMetadata]
lscrServerCertificateMetadataList = lens _lscrServerCertificateMetadataList (\ s a -> s{_lscrServerCertificateMetadataList = a});
