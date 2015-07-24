{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListServerCertificates
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the server certificates that have the specified path prefix. If
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
    , lscrsMarker
    , lscrsIsTruncated
    , lscrsStatus
    , lscrsServerCertificateMetadataList
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listServerCertificates' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscPathPrefix'
--
-- * 'lscMaxItems'
--
-- * 'lscMarker'
data ListServerCertificates = ListServerCertificates'
    { _lscPathPrefix :: !(Maybe Text)
    , _lscMaxItems   :: !(Maybe Nat)
    , _lscMarker     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListServerCertificates' smart constructor.
listServerCertificates :: ListServerCertificates
listServerCertificates =
    ListServerCertificates'
    { _lscPathPrefix = Nothing
    , _lscMaxItems = Nothing
    , _lscMarker = Nothing
    }

-- | The path prefix for filtering the results. For example:
-- @\/company\/servercerts@ would get all server certificates for which the
-- path starts with @\/company\/servercerts@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all server certificates.
lscPathPrefix :: Lens' ListServerCertificates (Maybe Text)
lscPathPrefix = lens _lscPathPrefix (\ s a -> s{_lscPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lscMaxItems :: Lens' ListServerCertificates (Maybe Natural)
lscMaxItems = lens _lscMaxItems (\ s a -> s{_lscMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lscMarker :: Lens' ListServerCertificates (Maybe Text)
lscMarker = lens _lscMarker (\ s a -> s{_lscMarker = a});

instance AWSPager ListServerCertificates where
        page rq rs
          | stop (rs ^. lscrsIsTruncated) = Nothing
          | isNothing (rs ^. lscrsMarker) = Nothing
          | otherwise =
            Just $ rq & lscMarker .~ rs ^. lscrsMarker

instance AWSRequest ListServerCertificates where
        type Sv ListServerCertificates = IAM
        type Rs ListServerCertificates =
             ListServerCertificatesResponse
        request = post
        response
          = receiveXMLWrapper "ListServerCertificatesResult"
              (\ s h x ->
                 ListServerCertificatesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
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

-- | Contains the response to a successful ListServerCertificates request.
--
-- /See:/ 'listServerCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscrsMarker'
--
-- * 'lscrsIsTruncated'
--
-- * 'lscrsStatus'
--
-- * 'lscrsServerCertificateMetadataList'
data ListServerCertificatesResponse = ListServerCertificatesResponse'
    { _lscrsMarker                        :: !(Maybe Text)
    , _lscrsIsTruncated                   :: !(Maybe Bool)
    , _lscrsStatus                        :: !Int
    , _lscrsServerCertificateMetadataList :: ![ServerCertificateMetadata]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListServerCertificatesResponse' smart constructor.
listServerCertificatesResponse :: Int -> ListServerCertificatesResponse
listServerCertificatesResponse pStatus_ =
    ListServerCertificatesResponse'
    { _lscrsMarker = Nothing
    , _lscrsIsTruncated = Nothing
    , _lscrsStatus = pStatus_
    , _lscrsServerCertificateMetadataList = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lscrsMarker :: Lens' ListServerCertificatesResponse (Maybe Text)
lscrsMarker = lens _lscrsMarker (\ s a -> s{_lscrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lscrsIsTruncated :: Lens' ListServerCertificatesResponse (Maybe Bool)
lscrsIsTruncated = lens _lscrsIsTruncated (\ s a -> s{_lscrsIsTruncated = a});

-- | FIXME: Undocumented member.
lscrsStatus :: Lens' ListServerCertificatesResponse Int
lscrsStatus = lens _lscrsStatus (\ s a -> s{_lscrsStatus = a});

-- | A list of server certificates.
lscrsServerCertificateMetadataList :: Lens' ListServerCertificatesResponse [ServerCertificateMetadata]
lscrsServerCertificateMetadataList = lens _lscrsServerCertificateMetadataList (\ s a -> s{_lscrsServerCertificateMetadataList = a});
