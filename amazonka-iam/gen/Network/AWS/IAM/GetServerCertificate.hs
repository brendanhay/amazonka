{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetServerCertificate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about the specified server certificate.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificate.html>
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
import qualified GHC.Exts

newtype GetServerCertificate = GetServerCertificate
    { _gscServerCertificateName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

-- | The name of the server certificate you want to retrieve information about.
gscServerCertificateName :: Lens' GetServerCertificate Text
gscServerCertificateName =
    lens _gscServerCertificateName
        (\s a -> s { _gscServerCertificateName = a })

newtype GetServerCertificateResponse = GetServerCertificateResponse
    { _gscrServerCertificate :: ServerCertificate
    } deriving (Eq, Read, Show)

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

instance ToPath GetServerCertificate where
    toPath = const "/"

instance ToQuery GetServerCertificate where
    toQuery GetServerCertificate{..} = mconcat
        [ "ServerCertificateName" =? _gscServerCertificateName
        ]

instance ToHeaders GetServerCertificate

instance AWSRequest GetServerCertificate where
    type Sv GetServerCertificate = IAM
    type Rs GetServerCertificate = GetServerCertificateResponse

    request  = post "GetServerCertificate"
    response = xmlResponse

instance FromXML GetServerCertificateResponse where
    parseXML = withElement "GetServerCertificateResult" $ \x -> GetServerCertificateResponse
        <$> x .@  "ServerCertificate"
