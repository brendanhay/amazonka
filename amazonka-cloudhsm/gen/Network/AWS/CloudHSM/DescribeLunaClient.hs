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

-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
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

-- | Retrieves information about an HSM client.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeLunaClient.html>
module Network.AWS.CloudHSM.DescribeLunaClient
    (
    -- * Request
      DescribeLunaClient
    -- ** Request constructor
    , describeLunaClient
    -- ** Request lenses
    , dlcCertificateFingerprint
    , dlcClientArn

    -- * Response
    , DescribeLunaClientResponse
    -- ** Response constructor
    , describeLunaClientResponse
    -- ** Response lenses
    , dlcrCertificate
    , dlcrCertificateFingerprint
    , dlcrClientArn
    , dlcrLabel
    , dlcrLastModifiedTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data DescribeLunaClient = DescribeLunaClient
    { _dlcCertificateFingerprint :: Maybe Text
    , _dlcClientArn              :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeLunaClient' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcCertificateFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'dlcClientArn' @::@ 'Maybe' 'Text'
--
describeLunaClient :: DescribeLunaClient
describeLunaClient = DescribeLunaClient
    { _dlcClientArn              = Nothing
    , _dlcCertificateFingerprint = Nothing
    }

-- | The certificate fingerprint.
dlcCertificateFingerprint :: Lens' DescribeLunaClient (Maybe Text)
dlcCertificateFingerprint =
    lens _dlcCertificateFingerprint
        (\s a -> s { _dlcCertificateFingerprint = a })

-- | The ARN of the client.
dlcClientArn :: Lens' DescribeLunaClient (Maybe Text)
dlcClientArn = lens _dlcClientArn (\s a -> s { _dlcClientArn = a })

data DescribeLunaClientResponse = DescribeLunaClientResponse
    { _dlcrCertificate            :: Maybe Text
    , _dlcrCertificateFingerprint :: Maybe Text
    , _dlcrClientArn              :: Maybe Text
    , _dlcrLabel                  :: Maybe Text
    , _dlcrLastModifiedTimestamp  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeLunaClientResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrCertificate' @::@ 'Maybe' 'Text'
--
-- * 'dlcrCertificateFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'dlcrClientArn' @::@ 'Maybe' 'Text'
--
-- * 'dlcrLabel' @::@ 'Maybe' 'Text'
--
-- * 'dlcrLastModifiedTimestamp' @::@ 'Maybe' 'Text'
--
describeLunaClientResponse :: DescribeLunaClientResponse
describeLunaClientResponse = DescribeLunaClientResponse
    { _dlcrClientArn              = Nothing
    , _dlcrCertificate            = Nothing
    , _dlcrCertificateFingerprint = Nothing
    , _dlcrLastModifiedTimestamp  = Nothing
    , _dlcrLabel                  = Nothing
    }

-- | The certificate installed on the HSMs used by this client.
dlcrCertificate :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrCertificate = lens _dlcrCertificate (\s a -> s { _dlcrCertificate = a })

-- | The certificate fingerprint.
dlcrCertificateFingerprint :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrCertificateFingerprint =
    lens _dlcrCertificateFingerprint
        (\s a -> s { _dlcrCertificateFingerprint = a })

-- | The ARN of the client.
dlcrClientArn :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrClientArn = lens _dlcrClientArn (\s a -> s { _dlcrClientArn = a })

-- | The label of the client.
dlcrLabel :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrLabel = lens _dlcrLabel (\s a -> s { _dlcrLabel = a })

-- | The date and time the client was last modified.
dlcrLastModifiedTimestamp :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrLastModifiedTimestamp =
    lens _dlcrLastModifiedTimestamp
        (\s a -> s { _dlcrLastModifiedTimestamp = a })

instance ToPath DescribeLunaClient where
    toPath = const "/"

instance ToQuery DescribeLunaClient where
    toQuery = const mempty

instance ToHeaders DescribeLunaClient

instance ToJSON DescribeLunaClient where
    toJSON DescribeLunaClient{..} = object
        [ "ClientArn"              .= _dlcClientArn
        , "CertificateFingerprint" .= _dlcCertificateFingerprint
        ]

instance AWSRequest DescribeLunaClient where
    type Sv DescribeLunaClient = CloudHSM
    type Rs DescribeLunaClient = DescribeLunaClientResponse

    request  = post "DescribeLunaClient"
    response = jsonResponse

instance FromJSON DescribeLunaClientResponse where
    parseJSON = withObject "DescribeLunaClientResponse" $ \o -> DescribeLunaClientResponse
        <$> o .:? "Certificate"
        <*> o .:? "CertificateFingerprint"
        <*> o .:? "ClientArn"
        <*> o .:? "Label"
        <*> o .:? "LastModifiedTimestamp"
