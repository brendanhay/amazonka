{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified server certificate.
module Network.AWS.IAM.GetServerCertificate
    (
    -- * Request
      GetServerCertificate
    -- ** Request constructor
    , getServerCertificate
    -- ** Request lenses
    , gscServerCertificateName

    -- * Response
    , GetServerCertificateResponse
    -- ** Response constructor
    , getServerCertificateResponse
    -- ** Response lenses
    , gscrServerCertificate
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

newtype GetServerCertificate = GetServerCertificate
    { _gscServerCertificateName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetServerCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gscServerCertificateName' @::@ 'Text'
--
getServerCertificate :: Text -- ^ 'gscServerCertificateName'
                     -> GetServerCertificate
getServerCertificate p1 = GetServerCertificate
    { _gscServerCertificateName = p1
    }

-- | The name of the server certificate you want to retrieve information
-- about.
gscServerCertificateName :: Lens' GetServerCertificate Text
gscServerCertificateName =
    lens _gscServerCertificateName
        (\s a -> s { _gscServerCertificateName = a })

instance ToQuery GetServerCertificate

instance ToPath GetServerCertificate where
    toPath = const "/"

newtype GetServerCertificateResponse = GetServerCertificateResponse
    { _gscrServerCertificate :: ServerCertificate
    } deriving (Eq, Show, Generic)

-- | 'GetServerCertificateResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gscrServerCertificate' @::@ 'ServerCertificate'
--
getServerCertificateResponse :: ServerCertificate -- ^ 'gscrServerCertificate'
                             -> GetServerCertificateResponse
getServerCertificateResponse p1 = GetServerCertificateResponse
    { _gscrServerCertificate = p1
    }

-- | Information about the server certificate.
gscrServerCertificate :: Lens' GetServerCertificateResponse ServerCertificate
gscrServerCertificate =
    lens _gscrServerCertificate (\s a -> s { _gscrServerCertificate = a })

instance FromXML GetServerCertificateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetServerCertificateResponse"

instance AWSRequest GetServerCertificate where
    type Sv GetServerCertificate = IAM
    type Rs GetServerCertificate = GetServerCertificateResponse

    request  = post "GetServerCertificate"
    response = xmlResponse $ \h x -> GetServerCertificateResponse
        <$> x %| "ServerCertificate"
