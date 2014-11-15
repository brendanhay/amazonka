{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.IAM.ListServerCertificates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the server certificates that have the specified path prefix. If none
-- exist, the action returns an empty list. You can paginate the results using
-- the MaxItems and Marker parameters.
module Network.AWS.IAM.ListServerCertificates
    (
    -- * Request
      ListServerCertificates
    -- ** Request constructor
    , listServerCertificates
    -- ** Request lenses
    , lscMarker
    , lscMaxItems
    , lscPathPrefix

    -- * Response
    , ListServerCertificatesResponse
    -- ** Response constructor
    , listServerCertificatesResponse
    -- ** Response lenses
    , lscrIsTruncated
    , lscrMarker
    , lscrServerCertificateMetadataList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data ListServerCertificates = ListServerCertificates
    { _lscMarker     :: Maybe Text
    , _lscMaxItems   :: Maybe Nat
    , _lscPathPrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListServerCertificates' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscMarker' @::@ 'Maybe' 'Text'
--
-- * 'lscMaxItems' @::@ 'Maybe' 'Natural'
--
-- * 'lscPathPrefix' @::@ 'Maybe' 'Text'
--
listServerCertificates :: ListServerCertificates
listServerCertificates = ListServerCertificates
    { _lscPathPrefix = Nothing
    , _lscMarker     = Nothing
    , _lscMaxItems   = Nothing
    }

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it
-- to the value of the Marker element in the response you just received.
lscMarker :: Lens' ListServerCertificates (Maybe Text)
lscMarker = lens _lscMarker (\s a -> s { _lscMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- server certificates you want in the response. If there are additional
-- server certificates beyond the maximum you specify, the IsTruncated
-- response element will be set to true. This parameter is optional. If you
-- do not include it, it defaults to 100.
lscMaxItems :: Lens' ListServerCertificates (Maybe Natural)
lscMaxItems = lens _lscMaxItems (\s a -> s { _lscMaxItems = a })
    . mapping _Nat

-- | The path prefix for filtering the results. For example:
-- /company/servercerts would get all server certificates for which the path
-- starts with /company/servercerts. This parameter is optional. If it is
-- not included, it defaults to a slash (/), listing all server
-- certificates.
lscPathPrefix :: Lens' ListServerCertificates (Maybe Text)
lscPathPrefix = lens _lscPathPrefix (\s a -> s { _lscPathPrefix = a })

instance ToQuery ListServerCertificates

instance ToPath ListServerCertificates where
    toPath = const "/"

data ListServerCertificatesResponse = ListServerCertificatesResponse
    { _lscrIsTruncated                   :: Maybe Bool
    , _lscrMarker                        :: Maybe Text
    , _lscrServerCertificateMetadataList :: [ServerCertificateMetadata]
    } deriving (Eq, Show, Generic)

-- | 'ListServerCertificatesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lscrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lscrMarker' @::@ 'Maybe' 'Text'
--
-- * 'lscrServerCertificateMetadataList' @::@ ['ServerCertificateMetadata']
--
listServerCertificatesResponse :: ListServerCertificatesResponse
listServerCertificatesResponse = ListServerCertificatesResponse
    { _lscrServerCertificateMetadataList = mempty
    , _lscrIsTruncated                   = Nothing
    , _lscrMarker                        = Nothing
    }

-- | A flag that indicates whether there are more server certificates to list.
-- If your results were truncated, you can make a subsequent pagination
-- request using the Marker request parameter to retrieve more server
-- certificates in the list.
lscrIsTruncated :: Lens' ListServerCertificatesResponse (Maybe Bool)
lscrIsTruncated = lens _lscrIsTruncated (\s a -> s { _lscrIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lscrMarker :: Lens' ListServerCertificatesResponse (Maybe Text)
lscrMarker = lens _lscrMarker (\s a -> s { _lscrMarker = a })

-- | A list of server certificates.
lscrServerCertificateMetadataList :: Lens' ListServerCertificatesResponse [ServerCertificateMetadata]
lscrServerCertificateMetadataList =
    lens _lscrServerCertificateMetadataList
        (\s a -> s { _lscrServerCertificateMetadataList = a })

instance AWSRequest ListServerCertificates where
    type Sv ListServerCertificates = IAM
    type Rs ListServerCertificates = ListServerCertificatesResponse

    request  = post "ListServerCertificates"
    response = xmlResponse $ \h x -> ListServerCertificatesResponse
        <$> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "ServerCertificateMetadataList"

instance AWSPager ListServerCertificates where
    next rq rs
        | not (more (rs ^. lscrIsTruncated)) = Nothing
        | otherwise = Just $ rq
            & lscMarker .~ rs ^. lscrMarker
