{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , dClientARN
    , dCertificateFingerprint
    , dLastModifiedTimestamp
    , dCertificate
    , dLabel
    , dStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcClientARN'
--
-- * 'dlcCertificateFingerprint'
data DescribeLunaClient = DescribeLunaClient'
    { _dlcClientARN              :: !(Maybe Text)
    , _dlcCertificateFingerprint :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLunaClient' smart constructor.
describeLunaClient :: DescribeLunaClient
describeLunaClient =
    DescribeLunaClient'
    { _dlcClientARN = Nothing
    , _dlcCertificateFingerprint = Nothing
    }

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
                   (x .?> "ClientArn") <*>
                     (x .?> "CertificateFingerprint")
                     <*> (x .?> "LastModifiedTimestamp")
                     <*> (x .?> "Certificate")
                     <*> (x .?> "Label")
                     <*> (pure (fromEnum s)))

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
-- * 'dClientARN'
--
-- * 'dCertificateFingerprint'
--
-- * 'dLastModifiedTimestamp'
--
-- * 'dCertificate'
--
-- * 'dLabel'
--
-- * 'dStatus'
data DescribeLunaClientResponse = DescribeLunaClientResponse'
    { _dClientARN              :: !(Maybe Text)
    , _dCertificateFingerprint :: !(Maybe Text)
    , _dLastModifiedTimestamp  :: !(Maybe Text)
    , _dCertificate            :: !(Maybe Text)
    , _dLabel                  :: !(Maybe Text)
    , _dStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLunaClientResponse' smart constructor.
describeLunaClientResponse :: Int -> DescribeLunaClientResponse
describeLunaClientResponse pStatus =
    DescribeLunaClientResponse'
    { _dClientARN = Nothing
    , _dCertificateFingerprint = Nothing
    , _dLastModifiedTimestamp = Nothing
    , _dCertificate = Nothing
    , _dLabel = Nothing
    , _dStatus = pStatus
    }

-- | The ARN of the client.
dClientARN :: Lens' DescribeLunaClientResponse (Maybe Text)
dClientARN = lens _dClientARN (\ s a -> s{_dClientARN = a});

-- | The certificate fingerprint.
dCertificateFingerprint :: Lens' DescribeLunaClientResponse (Maybe Text)
dCertificateFingerprint = lens _dCertificateFingerprint (\ s a -> s{_dCertificateFingerprint = a});

-- | The date and time the client was last modified.
dLastModifiedTimestamp :: Lens' DescribeLunaClientResponse (Maybe Text)
dLastModifiedTimestamp = lens _dLastModifiedTimestamp (\ s a -> s{_dLastModifiedTimestamp = a});

-- | The certificate installed on the HSMs used by this client.
dCertificate :: Lens' DescribeLunaClientResponse (Maybe Text)
dCertificate = lens _dCertificate (\ s a -> s{_dCertificate = a});

-- | The label of the client.
dLabel :: Lens' DescribeLunaClientResponse (Maybe Text)
dLabel = lens _dLabel (\ s a -> s{_dLabel = a});

-- | FIXME: Undocumented member.
dStatus :: Lens' DescribeLunaClientResponse Int
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});
