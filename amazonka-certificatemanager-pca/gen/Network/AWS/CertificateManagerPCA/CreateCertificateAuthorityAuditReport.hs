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
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an audit report that lists every time that the your CA private key is used. The report is saved in the Amazon S3 bucket that you specify on input. The 'IssueCertificate' and 'RevokeCertificate' functions use the private key. You can generate a new report every 30 minutes.
--
--
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
    (
    -- * Creating a Request
      createCertificateAuthorityAuditReport
    , CreateCertificateAuthorityAuditReport
    -- * Request Lenses
    , ccaarCertificateAuthorityARN
    , ccaarS3BucketName
    , ccaarAuditReportResponseFormat

    -- * Destructuring the Response
    , createCertificateAuthorityAuditReportResponse
    , CreateCertificateAuthorityAuditReportResponse
    -- * Response Lenses
    , ccaarrsS3Key
    , ccaarrsAuditReportId
    , ccaarrsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCertificateAuthorityAuditReport' smart constructor.
data CreateCertificateAuthorityAuditReport = CreateCertificateAuthorityAuditReport'
  { _ccaarCertificateAuthorityARN   :: !Text
  , _ccaarS3BucketName              :: !Text
  , _ccaarAuditReportResponseFormat :: !AuditReportResponseFormat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCertificateAuthorityAuditReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccaarCertificateAuthorityARN' - Amazon Resource Name (ARN) of the CA to be audited. This is of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'ccaarS3BucketName' - Name of the S3 bucket that will contain the audit report.
--
-- * 'ccaarAuditReportResponseFormat' - Format in which to create the report. This can be either __JSON__ or __CSV__ .
createCertificateAuthorityAuditReport
    :: Text -- ^ 'ccaarCertificateAuthorityARN'
    -> Text -- ^ 'ccaarS3BucketName'
    -> AuditReportResponseFormat -- ^ 'ccaarAuditReportResponseFormat'
    -> CreateCertificateAuthorityAuditReport
createCertificateAuthorityAuditReport pCertificateAuthorityARN_ pS3BucketName_ pAuditReportResponseFormat_ =
  CreateCertificateAuthorityAuditReport'
    { _ccaarCertificateAuthorityARN = pCertificateAuthorityARN_
    , _ccaarS3BucketName = pS3BucketName_
    , _ccaarAuditReportResponseFormat = pAuditReportResponseFormat_
    }


-- | Amazon Resource Name (ARN) of the CA to be audited. This is of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
ccaarCertificateAuthorityARN :: Lens' CreateCertificateAuthorityAuditReport Text
ccaarCertificateAuthorityARN = lens _ccaarCertificateAuthorityARN (\ s a -> s{_ccaarCertificateAuthorityARN = a})

-- | Name of the S3 bucket that will contain the audit report.
ccaarS3BucketName :: Lens' CreateCertificateAuthorityAuditReport Text
ccaarS3BucketName = lens _ccaarS3BucketName (\ s a -> s{_ccaarS3BucketName = a})

-- | Format in which to create the report. This can be either __JSON__ or __CSV__ .
ccaarAuditReportResponseFormat :: Lens' CreateCertificateAuthorityAuditReport AuditReportResponseFormat
ccaarAuditReportResponseFormat = lens _ccaarAuditReportResponseFormat (\ s a -> s{_ccaarAuditReportResponseFormat = a})

instance AWSRequest
           CreateCertificateAuthorityAuditReport
         where
        type Rs CreateCertificateAuthorityAuditReport =
             CreateCertificateAuthorityAuditReportResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 CreateCertificateAuthorityAuditReportResponse' <$>
                   (x .?> "S3Key") <*> (x .?> "AuditReportId") <*>
                     (pure (fromEnum s)))

instance Hashable
           CreateCertificateAuthorityAuditReport
         where

instance NFData CreateCertificateAuthorityAuditReport
         where

instance ToHeaders
           CreateCertificateAuthorityAuditReport
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.CreateCertificateAuthorityAuditReport"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCertificateAuthorityAuditReport
         where
        toJSON CreateCertificateAuthorityAuditReport'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _ccaarCertificateAuthorityARN),
                  Just ("S3BucketName" .= _ccaarS3BucketName),
                  Just
                    ("AuditReportResponseFormat" .=
                       _ccaarAuditReportResponseFormat)])

instance ToPath CreateCertificateAuthorityAuditReport
         where
        toPath = const "/"

instance ToQuery
           CreateCertificateAuthorityAuditReport
         where
        toQuery = const mempty

-- | /See:/ 'createCertificateAuthorityAuditReportResponse' smart constructor.
data CreateCertificateAuthorityAuditReportResponse = CreateCertificateAuthorityAuditReportResponse'
  { _ccaarrsS3Key          :: !(Maybe Text)
  , _ccaarrsAuditReportId  :: !(Maybe Text)
  , _ccaarrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCertificateAuthorityAuditReportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccaarrsS3Key' - The __key__ that uniquely identifies the report file in your S3 bucket.
--
-- * 'ccaarrsAuditReportId' - An alphanumeric string that contains a report identifier.
--
-- * 'ccaarrsResponseStatus' - -- | The response status code.
createCertificateAuthorityAuditReportResponse
    :: Int -- ^ 'ccaarrsResponseStatus'
    -> CreateCertificateAuthorityAuditReportResponse
createCertificateAuthorityAuditReportResponse pResponseStatus_ =
  CreateCertificateAuthorityAuditReportResponse'
    { _ccaarrsS3Key = Nothing
    , _ccaarrsAuditReportId = Nothing
    , _ccaarrsResponseStatus = pResponseStatus_
    }


-- | The __key__ that uniquely identifies the report file in your S3 bucket.
ccaarrsS3Key :: Lens' CreateCertificateAuthorityAuditReportResponse (Maybe Text)
ccaarrsS3Key = lens _ccaarrsS3Key (\ s a -> s{_ccaarrsS3Key = a})

-- | An alphanumeric string that contains a report identifier.
ccaarrsAuditReportId :: Lens' CreateCertificateAuthorityAuditReportResponse (Maybe Text)
ccaarrsAuditReportId = lens _ccaarrsAuditReportId (\ s a -> s{_ccaarrsAuditReportId = a})

-- | -- | The response status code.
ccaarrsResponseStatus :: Lens' CreateCertificateAuthorityAuditReportResponse Int
ccaarrsResponseStatus = lens _ccaarrsResponseStatus (\ s a -> s{_ccaarrsResponseStatus = a})

instance NFData
           CreateCertificateAuthorityAuditReportResponse
         where
