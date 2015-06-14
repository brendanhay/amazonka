{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
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
    , dlcClientARN
    , dlcCertificateFingerprint

    -- * Response
    , DescribeLunaClientResponse
    -- ** Response constructor
    , describeLunaClientResponse
    -- ** Response lenses
    , dlcrClientARN
    , dlcrCertificateFingerprint
    , dlcrLastModifiedTimestamp
    , dlcrLabel
    , dlcrCertificate
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'describeLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcClientARN'
--
-- * 'dlcCertificateFingerprint'
data DescribeLunaClient = DescribeLunaClient'{_dlcClientARN :: Maybe Text, _dlcCertificateFingerprint :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeLunaClient' smart constructor.
describeLunaClient :: DescribeLunaClient
describeLunaClient = DescribeLunaClient'{_dlcClientARN = Nothing, _dlcCertificateFingerprint = Nothing};

-- | The ARN of the client.
dlcClientARN :: Lens' DescribeLunaClient (Maybe Text)
dlcClientARN = lens _dlcClientARN (\ s a -> s{_dlcClientARN = a});

-- | The certificate fingerprint.
dlcCertificateFingerprint :: Lens' DescribeLunaClient (Maybe Text)
dlcCertificateFingerprint = lens _dlcCertificateFingerprint (\ s a -> s{_dlcCertificateFingerprint = a});

instance AWSRequest DescribeLunaClient where
        type Sv DescribeLunaClient = CloudHSM
        type Rs DescribeLunaClient =
             DescribeLunaClientResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLunaClientResponse' <$>
                   x .?> "ClientArn" <*> x .?> "CertificateFingerprint"
                     <*> x .?> "LastModifiedTimestamp"
                     <*> x .?> "Label"
                     <*> x .:> "Certificate")

instance ToHeaders DescribeLunaClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DescribeLunaClient" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLunaClient where
        toJSON DescribeLunaClient'{..}
          = object
              ["ClientArn" .= _dlcClientARN,
               "CertificateFingerprint" .=
                 _dlcCertificateFingerprint]

instance ToPath DescribeLunaClient where
        toPath = const "/"

instance ToQuery DescribeLunaClient where
        toQuery = const mempty

-- | /See:/ 'describeLunaClientResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrClientARN'
--
-- * 'dlcrCertificateFingerprint'
--
-- * 'dlcrLastModifiedTimestamp'
--
-- * 'dlcrLabel'
--
-- * 'dlcrCertificate'
data DescribeLunaClientResponse = DescribeLunaClientResponse'{_dlcrClientARN :: Maybe Text, _dlcrCertificateFingerprint :: Maybe Text, _dlcrLastModifiedTimestamp :: Maybe Text, _dlcrLabel :: Maybe Text, _dlcrCertificate :: Text} deriving (Eq, Read, Show)

-- | 'DescribeLunaClientResponse' smart constructor.
describeLunaClientResponse :: Text -> DescribeLunaClientResponse
describeLunaClientResponse pCertificate = DescribeLunaClientResponse'{_dlcrClientARN = Nothing, _dlcrCertificateFingerprint = Nothing, _dlcrLastModifiedTimestamp = Nothing, _dlcrLabel = Nothing, _dlcrCertificate = pCertificate};

-- | The ARN of the client.
dlcrClientARN :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrClientARN = lens _dlcrClientARN (\ s a -> s{_dlcrClientARN = a});

-- | The certificate fingerprint.
dlcrCertificateFingerprint :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrCertificateFingerprint = lens _dlcrCertificateFingerprint (\ s a -> s{_dlcrCertificateFingerprint = a});

-- | The date and time the client was last modified.
dlcrLastModifiedTimestamp :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrLastModifiedTimestamp = lens _dlcrLastModifiedTimestamp (\ s a -> s{_dlcrLastModifiedTimestamp = a});

-- | The label of the client.
dlcrLabel :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrLabel = lens _dlcrLabel (\ s a -> s{_dlcrLabel = a});

-- | The certificate installed on the HSMs used by this client.
dlcrCertificate :: Lens' DescribeLunaClientResponse Text
dlcrCertificate = lens _dlcrCertificate (\ s a -> s{_dlcrCertificate = a});
