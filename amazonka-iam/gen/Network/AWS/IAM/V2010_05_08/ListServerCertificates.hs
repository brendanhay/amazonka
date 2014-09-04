{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListServerCertificates
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
-- the MaxItems and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListServerCertificates &PathPrefix=/company/servercerts
-- &Version=2010-05-08 &AUTHPARAMS false ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6CEXAMPLE1 2012-05-08T01:02:03.004Z
-- BetaServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/BetaServerCert
-- 2010-05-08T02:03:01.004Z ASCACKCEVSQ6CEXAMPLE2 2012-05-08T02:03:01.004Z
-- TestServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/TestServerCert
-- 2010-05-08T03:01:02.004Z ASCACKCEVSQ6CEXAMPLE3 2012-05-08T03:01:02.004Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListServerCertificates
    (
    -- * Request
      ListServerCertificates
    -- ** Request constructor
    , mkListServerCertificatesRequest
    -- ** Request lenses
    , lscrPathPrefix
    , lscrMarker
    , lscrMaxItems

    -- * Response
    , ListServerCertificatesResponse
    -- ** Response lenses
    , lscsServerCertificateMetadataList
    , lscsIsTruncated
    , lscsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListServerCertificates' request.
mkListServerCertificatesRequest :: ListServerCertificates
mkListServerCertificatesRequest = ListServerCertificates
    { _lscrPathPrefix = Nothing
    , _lscrMarker = Nothing
    , _lscrMaxItems = Nothing
    }
{-# INLINE mkListServerCertificatesRequest #-}

data ListServerCertificates = ListServerCertificates
    { _lscrPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /company/servercerts would get all server certificates for which
      -- the path starts with /company/servercerts. This parameter is
      -- optional. If it is not included, it defaults to a slash (/),
      -- listing all server certificates.
    , _lscrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _lscrMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of server certificates you want in the response. If there
      -- are additional server certificates beyond the maximum you
      -- specify, the IsTruncated response element will be set to true.
      -- This parameter is optional. If you do not include it, it defaults
      -- to 100.
    } deriving (Show, Generic)

-- | The path prefix for filtering the results. For example:
-- /company/servercerts would get all server certificates for which the path
-- starts with /company/servercerts. This parameter is optional. If it is not
-- included, it defaults to a slash (/), listing all server certificates.
lscrPathPrefix :: Lens' ListServerCertificates (Maybe Text)
lscrPathPrefix = lens _lscrPathPrefix (\s a -> s { _lscrPathPrefix = a })
{-# INLINE lscrPathPrefix #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lscrMarker :: Lens' ListServerCertificates (Maybe Text)
lscrMarker = lens _lscrMarker (\s a -> s { _lscrMarker = a })
{-# INLINE lscrMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- server certificates you want in the response. If there are additional
-- server certificates beyond the maximum you specify, the IsTruncated
-- response element will be set to true. This parameter is optional. If you do
-- not include it, it defaults to 100.
lscrMaxItems :: Lens' ListServerCertificates (Maybe Integer)
lscrMaxItems = lens _lscrMaxItems (\s a -> s { _lscrMaxItems = a })
{-# INLINE lscrMaxItems #-}

instance ToQuery ListServerCertificates where
    toQuery = genericQuery def

data ListServerCertificatesResponse = ListServerCertificatesResponse
    { _lscsServerCertificateMetadataList :: [ServerCertificateMetadata]
      -- ^ A list of server certificates.
    , _lscsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more server certificates
      -- to list. If your results were truncated, you can make a
      -- subsequent pagination request using the Marker request parameter
      -- to retrieve more server certificates in the list.
    , _lscsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

-- | A list of server certificates.
lscsServerCertificateMetadataList :: Lens' ListServerCertificatesResponse ([ServerCertificateMetadata])
lscsServerCertificateMetadataList = lens _lscsServerCertificateMetadataList (\s a -> s { _lscsServerCertificateMetadataList = a })
{-# INLINE lscsServerCertificateMetadataList #-}

-- | A flag that indicates whether there are more server certificates to list.
-- If your results were truncated, you can make a subsequent pagination
-- request using the Marker request parameter to retrieve more server
-- certificates in the list.
lscsIsTruncated :: Lens' ListServerCertificatesResponse (Bool)
lscsIsTruncated = lens _lscsIsTruncated (\s a -> s { _lscsIsTruncated = a })
{-# INLINE lscsIsTruncated #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lscsMarker :: Lens' ListServerCertificatesResponse (Maybe Text)
lscsMarker = lens _lscsMarker (\s a -> s { _lscsMarker = a })
{-# INLINE lscsMarker #-}

instance FromXML ListServerCertificatesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListServerCertificates where
    type Sv ListServerCertificates = IAM
    type Rs ListServerCertificates = ListServerCertificatesResponse

    request = post "ListServerCertificates"
    response _ = xmlResponse

instance AWSPager ListServerCertificates where
    next rq rs
        | not (_lscsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lscrMarker = _lscsMarker rs
            }
