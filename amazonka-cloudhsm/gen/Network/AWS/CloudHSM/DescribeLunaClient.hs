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
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DescribeLunaClient.html AWS API Reference> for DescribeLunaClient.
module Network.AWS.CloudHSM.DescribeLunaClient
    (
    -- * Creating a Request
      DescribeLunaClient
    , describeLunaClient
    -- * Request Lenses
    , dlcClientARN
    , dlcCertificateFingerprint

    -- * Destructuring the Response
    , DescribeLunaClientResponse
    , describeLunaClientResponse
    -- * Response Lenses
    , drsClientARN
    , drsCertificateFingerprint
    , drsLastModifiedTimestamp
    , drsCertificate
    , drsLabel
    , drsStatus
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
-- * 'drsClientARN'
--
-- * 'drsCertificateFingerprint'
--
-- * 'drsLastModifiedTimestamp'
--
-- * 'drsCertificate'
--
-- * 'drsLabel'
--
-- * 'drsStatus'
data DescribeLunaClientResponse = DescribeLunaClientResponse'
    { _drsClientARN              :: !(Maybe Text)
    , _drsCertificateFingerprint :: !(Maybe Text)
    , _drsLastModifiedTimestamp  :: !(Maybe Text)
    , _drsCertificate            :: !(Maybe Text)
    , _drsLabel                  :: !(Maybe Text)
    , _drsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeLunaClientResponse' smart constructor.
describeLunaClientResponse :: Int -> DescribeLunaClientResponse
describeLunaClientResponse pStatus_ =
    DescribeLunaClientResponse'
    { _drsClientARN = Nothing
    , _drsCertificateFingerprint = Nothing
    , _drsLastModifiedTimestamp = Nothing
    , _drsCertificate = Nothing
    , _drsLabel = Nothing
    , _drsStatus = pStatus_
    }

-- | The ARN of the client.
drsClientARN :: Lens' DescribeLunaClientResponse (Maybe Text)
drsClientARN = lens _drsClientARN (\ s a -> s{_drsClientARN = a});

-- | The certificate fingerprint.
drsCertificateFingerprint :: Lens' DescribeLunaClientResponse (Maybe Text)
drsCertificateFingerprint = lens _drsCertificateFingerprint (\ s a -> s{_drsCertificateFingerprint = a});

-- | The date and time the client was last modified.
drsLastModifiedTimestamp :: Lens' DescribeLunaClientResponse (Maybe Text)
drsLastModifiedTimestamp = lens _drsLastModifiedTimestamp (\ s a -> s{_drsLastModifiedTimestamp = a});

-- | The certificate installed on the HSMs used by this client.
drsCertificate :: Lens' DescribeLunaClientResponse (Maybe Text)
drsCertificate = lens _drsCertificate (\ s a -> s{_drsCertificate = a});

-- | The label of the client.
drsLabel :: Lens' DescribeLunaClientResponse (Maybe Text)
drsLabel = lens _drsLabel (\ s a -> s{_drsLabel = a});

-- | Undocumented member.
drsStatus :: Lens' DescribeLunaClientResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
