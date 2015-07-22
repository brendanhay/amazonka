{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an HSM client.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeLunaClient.html>
module Network.AWS.CloudHSM.DescribeLunaClient
    (
    -- * Request
      DescribeLunaClient
    -- ** Request constructor
    , describeLunaClient
    -- ** Request lenses
    , dlcrqClientARN
    , dlcrqCertificateFingerprint

    -- * Response
    , DescribeLunaClientResponse
    -- ** Response constructor
    , describeLunaClientResponse
    -- ** Response lenses
    , dlcrsClientARN
    , dlcrsCertificateFingerprint
    , dlcrsLastModifiedTimestamp
    , dlcrsCertificate
    , dlcrsLabel
    , dlcrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeLunaClient' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrqClientARN'
--
-- * 'dlcrqCertificateFingerprint'
data DescribeLunaClient = DescribeLunaClient'
    { _dlcrqClientARN              :: !(Maybe Text)
    , _dlcrqCertificateFingerprint :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLunaClient' smart constructor.
describeLunaClient :: DescribeLunaClient
describeLunaClient =
    DescribeLunaClient'
    { _dlcrqClientARN = Nothing
    , _dlcrqCertificateFingerprint = Nothing
    }

-- | The ARN of the client.
dlcrqClientARN :: Lens' DescribeLunaClient (Maybe Text)
dlcrqClientARN = lens _dlcrqClientARN (\ s a -> s{_dlcrqClientARN = a});

-- | The certificate fingerprint.
dlcrqCertificateFingerprint :: Lens' DescribeLunaClient (Maybe Text)
dlcrqCertificateFingerprint = lens _dlcrqCertificateFingerprint (\ s a -> s{_dlcrqCertificateFingerprint = a});

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
              ["ClientArn" .= _dlcrqClientARN,
               "CertificateFingerprint" .=
                 _dlcrqCertificateFingerprint]

instance ToPath DescribeLunaClient where
        toPath = const "/"

instance ToQuery DescribeLunaClient where
        toQuery = const mempty

-- | /See:/ 'describeLunaClientResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlcrsClientARN'
--
-- * 'dlcrsCertificateFingerprint'
--
-- * 'dlcrsLastModifiedTimestamp'
--
-- * 'dlcrsCertificate'
--
-- * 'dlcrsLabel'
--
-- * 'dlcrsStatus'
data DescribeLunaClientResponse = DescribeLunaClientResponse'
    { _dlcrsClientARN              :: !(Maybe Text)
    , _dlcrsCertificateFingerprint :: !(Maybe Text)
    , _dlcrsLastModifiedTimestamp  :: !(Maybe Text)
    , _dlcrsCertificate            :: !(Maybe Text)
    , _dlcrsLabel                  :: !(Maybe Text)
    , _dlcrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLunaClientResponse' smart constructor.
describeLunaClientResponse :: Int -> DescribeLunaClientResponse
describeLunaClientResponse pStatus =
    DescribeLunaClientResponse'
    { _dlcrsClientARN = Nothing
    , _dlcrsCertificateFingerprint = Nothing
    , _dlcrsLastModifiedTimestamp = Nothing
    , _dlcrsCertificate = Nothing
    , _dlcrsLabel = Nothing
    , _dlcrsStatus = pStatus
    }

-- | The ARN of the client.
dlcrsClientARN :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrsClientARN = lens _dlcrsClientARN (\ s a -> s{_dlcrsClientARN = a});

-- | The certificate fingerprint.
dlcrsCertificateFingerprint :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrsCertificateFingerprint = lens _dlcrsCertificateFingerprint (\ s a -> s{_dlcrsCertificateFingerprint = a});

-- | The date and time the client was last modified.
dlcrsLastModifiedTimestamp :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrsLastModifiedTimestamp = lens _dlcrsLastModifiedTimestamp (\ s a -> s{_dlcrsLastModifiedTimestamp = a});

-- | The certificate installed on the HSMs used by this client.
dlcrsCertificate :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrsCertificate = lens _dlcrsCertificate (\ s a -> s{_dlcrsCertificate = a});

-- | The label of the client.
dlcrsLabel :: Lens' DescribeLunaClientResponse (Maybe Text)
dlcrsLabel = lens _dlcrsLabel (\ s a -> s{_dlcrsLabel = a});

-- | FIXME: Undocumented member.
dlcrsStatus :: Lens' DescribeLunaClientResponse Int
dlcrsStatus = lens _dlcrsStatus (\ s a -> s{_dlcrsStatus = a});
