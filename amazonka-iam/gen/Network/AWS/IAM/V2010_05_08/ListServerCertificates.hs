{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.IAM.V2010_05_08.ListServerCertificates where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListServerCertificates' request.
listServerCertificates :: ListServerCertificates
listServerCertificates = ListServerCertificates
    { _lscrMarker = Nothing
    , _lscrMaxItems = Nothing
    , _lscrPathPrefix = Nothing
    }

data ListServerCertificates = ListServerCertificates
    { _lscrMarker :: Maybe Text
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
    , _lscrPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /company/servercerts would get all server certificates for which
      -- the path starts with /company/servercerts. This parameter is
      -- optional. If it is not included, it defaults to a slash (/),
      -- listing all server certificates.
    } deriving (Show, Generic)

makeLenses ''ListServerCertificates

instance ToQuery ListServerCertificates where
    toQuery = genericQuery def

data ListServerCertificatesResponse = ListServerCertificatesResponse
    { _lscsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more server certificates
      -- to list. If your results were truncated, you can make a
      -- subsequent pagination request using the Marker request parameter
      -- to retrieve more server certificates in the list.
    , _lscsServerCertificateMetadataList :: [ServerCertificateMetadata]
      -- ^ A list of server certificates.
    , _lscsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

makeLenses ''ListServerCertificatesResponse

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
