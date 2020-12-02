{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a specific audit report created by calling the 'CreateCertificateAuthorityAuditReport' function. Audit information is created every time the certificate authority (CA) private key is used. The private key is used when you call the 'IssueCertificate' function or the 'RevokeCertificate' function.
--
--
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
    (
    -- * Creating a Request
      describeCertificateAuthorityAuditReport
    , DescribeCertificateAuthorityAuditReport
    -- * Request Lenses
    , dcaarCertificateAuthorityARN
    , dcaarAuditReportId

    -- * Destructuring the Response
    , describeCertificateAuthorityAuditReportResponse
    , DescribeCertificateAuthorityAuditReportResponse
    -- * Response Lenses
    , dcaarrsS3Key
    , dcaarrsCreatedAt
    , dcaarrsAuditReportStatus
    , dcaarrsS3BucketName
    , dcaarrsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCertificateAuthorityAuditReport' smart constructor.
data DescribeCertificateAuthorityAuditReport = DescribeCertificateAuthorityAuditReport'
  { _dcaarCertificateAuthorityARN :: !Text
  , _dcaarAuditReportId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificateAuthorityAuditReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaarCertificateAuthorityARN' - The Amazon Resource Name (ARN) of the private CA. This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'dcaarAuditReportId' - The report ID returned by calling the 'CreateCertificateAuthorityAuditReport' function.
describeCertificateAuthorityAuditReport
    :: Text -- ^ 'dcaarCertificateAuthorityARN'
    -> Text -- ^ 'dcaarAuditReportId'
    -> DescribeCertificateAuthorityAuditReport
describeCertificateAuthorityAuditReport pCertificateAuthorityARN_ pAuditReportId_ =
  DescribeCertificateAuthorityAuditReport'
    { _dcaarCertificateAuthorityARN = pCertificateAuthorityARN_
    , _dcaarAuditReportId = pAuditReportId_
    }


-- | The Amazon Resource Name (ARN) of the private CA. This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
dcaarCertificateAuthorityARN :: Lens' DescribeCertificateAuthorityAuditReport Text
dcaarCertificateAuthorityARN = lens _dcaarCertificateAuthorityARN (\ s a -> s{_dcaarCertificateAuthorityARN = a})

-- | The report ID returned by calling the 'CreateCertificateAuthorityAuditReport' function.
dcaarAuditReportId :: Lens' DescribeCertificateAuthorityAuditReport Text
dcaarAuditReportId = lens _dcaarAuditReportId (\ s a -> s{_dcaarAuditReportId = a})

instance AWSRequest
           DescribeCertificateAuthorityAuditReport
         where
        type Rs DescribeCertificateAuthorityAuditReport =
             DescribeCertificateAuthorityAuditReportResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCertificateAuthorityAuditReportResponse' <$>
                   (x .?> "S3Key") <*> (x .?> "CreatedAt") <*>
                     (x .?> "AuditReportStatus")
                     <*> (x .?> "S3BucketName")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeCertificateAuthorityAuditReport
         where

instance NFData
           DescribeCertificateAuthorityAuditReport
         where

instance ToHeaders
           DescribeCertificateAuthorityAuditReport
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.DescribeCertificateAuthorityAuditReport"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeCertificateAuthorityAuditReport
         where
        toJSON DescribeCertificateAuthorityAuditReport'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _dcaarCertificateAuthorityARN),
                  Just ("AuditReportId" .= _dcaarAuditReportId)])

instance ToPath
           DescribeCertificateAuthorityAuditReport
         where
        toPath = const "/"

instance ToQuery
           DescribeCertificateAuthorityAuditReport
         where
        toQuery = const mempty

-- | /See:/ 'describeCertificateAuthorityAuditReportResponse' smart constructor.
data DescribeCertificateAuthorityAuditReportResponse = DescribeCertificateAuthorityAuditReportResponse'
  { _dcaarrsS3Key             :: !(Maybe Text)
  , _dcaarrsCreatedAt         :: !(Maybe POSIX)
  , _dcaarrsAuditReportStatus :: !(Maybe AuditReportStatus)
  , _dcaarrsS3BucketName      :: !(Maybe Text)
  , _dcaarrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificateAuthorityAuditReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaarrsS3Key' - S3 __key__ that uniquely identifies the report file in your S3 bucket.
--
-- * 'dcaarrsCreatedAt' - The date and time at which the report was created.
--
-- * 'dcaarrsAuditReportStatus' - Specifies whether report creation is in progress, has succeeded, or has failed.
--
-- * 'dcaarrsS3BucketName' - Name of the S3 bucket that contains the report.
--
-- * 'dcaarrsResponseStatus' - -- | The response status code.
describeCertificateAuthorityAuditReportResponse
    :: Int -- ^ 'dcaarrsResponseStatus'
    -> DescribeCertificateAuthorityAuditReportResponse
describeCertificateAuthorityAuditReportResponse pResponseStatus_ =
  DescribeCertificateAuthorityAuditReportResponse'
    { _dcaarrsS3Key = Nothing
    , _dcaarrsCreatedAt = Nothing
    , _dcaarrsAuditReportStatus = Nothing
    , _dcaarrsS3BucketName = Nothing
    , _dcaarrsResponseStatus = pResponseStatus_
    }


-- | S3 __key__ that uniquely identifies the report file in your S3 bucket.
dcaarrsS3Key :: Lens' DescribeCertificateAuthorityAuditReportResponse (Maybe Text)
dcaarrsS3Key = lens _dcaarrsS3Key (\ s a -> s{_dcaarrsS3Key = a})

-- | The date and time at which the report was created.
dcaarrsCreatedAt :: Lens' DescribeCertificateAuthorityAuditReportResponse (Maybe UTCTime)
dcaarrsCreatedAt = lens _dcaarrsCreatedAt (\ s a -> s{_dcaarrsCreatedAt = a}) . mapping _Time

-- | Specifies whether report creation is in progress, has succeeded, or has failed.
dcaarrsAuditReportStatus :: Lens' DescribeCertificateAuthorityAuditReportResponse (Maybe AuditReportStatus)
dcaarrsAuditReportStatus = lens _dcaarrsAuditReportStatus (\ s a -> s{_dcaarrsAuditReportStatus = a})

-- | Name of the S3 bucket that contains the report.
dcaarrsS3BucketName :: Lens' DescribeCertificateAuthorityAuditReportResponse (Maybe Text)
dcaarrsS3BucketName = lens _dcaarrsS3BucketName (\ s a -> s{_dcaarrsS3BucketName = a})

-- | -- | The response status code.
dcaarrsResponseStatus :: Lens' DescribeCertificateAuthorityAuditReportResponse Int
dcaarrsResponseStatus = lens _dcaarrsResponseStatus (\ s a -> s{_dcaarrsResponseStatus = a})

instance NFData
           DescribeCertificateAuthorityAuditReportResponse
         where
