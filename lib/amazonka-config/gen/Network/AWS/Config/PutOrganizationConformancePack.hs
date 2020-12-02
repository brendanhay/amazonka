{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutOrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys conformance packs across member accounts in an AWS Organization.
--
--
-- Only a master account and a delegated administrator can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
--
-- This API enables organization service access for @config-multiaccountsetup.amazonaws.com@ through the @EnableAWSServiceAccess@ action and creates a service linked role @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated administrator account of your organization. The service linked role is created only when the role does not exist in the caller account. To use this API with delegated administrator, register a delegated administrator by calling AWS Organization @register-delegate-admin@ for @config-multiaccountsetup.amazonaws.com@ .
module Network.AWS.Config.PutOrganizationConformancePack
  ( -- * Creating a Request
    putOrganizationConformancePack,
    PutOrganizationConformancePack,

    -- * Request Lenses
    pocpDeliveryS3Bucket,
    pocpDeliveryS3KeyPrefix,
    pocpTemplateS3URI,
    pocpConformancePackInputParameters,
    pocpExcludedAccounts,
    pocpTemplateBody,
    pocpOrganizationConformancePackName,

    -- * Destructuring the Response
    putOrganizationConformancePackResponse,
    PutOrganizationConformancePackResponse,

    -- * Response Lenses
    pocprsOrganizationConformancePackARN,
    pocprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putOrganizationConformancePack' smart constructor.
data PutOrganizationConformancePack = PutOrganizationConformancePack'
  { _pocpDeliveryS3Bucket ::
      !(Maybe Text),
    _pocpDeliveryS3KeyPrefix ::
      !(Maybe Text),
    _pocpTemplateS3URI ::
      !(Maybe Text),
    _pocpConformancePackInputParameters ::
      !( Maybe
           [ConformancePackInputParameter]
       ),
    _pocpExcludedAccounts ::
      !(Maybe [Text]),
    _pocpTemplateBody ::
      !(Maybe Text),
    _pocpOrganizationConformancePackName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutOrganizationConformancePack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocpDeliveryS3Bucket' - Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template.  The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
--
-- * 'pocpDeliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- * 'pocpTemplateS3URI' - Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
--
-- * 'pocpConformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- * 'pocpExcludedAccounts' - A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
--
-- * 'pocpTemplateBody' - A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- * 'pocpOrganizationConformancePackName' - Name of the organization conformance pack you want to create.
putOrganizationConformancePack ::
  -- | 'pocpOrganizationConformancePackName'
  Text ->
  PutOrganizationConformancePack
putOrganizationConformancePack pOrganizationConformancePackName_ =
  PutOrganizationConformancePack'
    { _pocpDeliveryS3Bucket = Nothing,
      _pocpDeliveryS3KeyPrefix = Nothing,
      _pocpTemplateS3URI = Nothing,
      _pocpConformancePackInputParameters = Nothing,
      _pocpExcludedAccounts = Nothing,
      _pocpTemplateBody = Nothing,
      _pocpOrganizationConformancePackName =
        pOrganizationConformancePackName_
    }

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template.  The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
pocpDeliveryS3Bucket :: Lens' PutOrganizationConformancePack (Maybe Text)
pocpDeliveryS3Bucket = lens _pocpDeliveryS3Bucket (\s a -> s {_pocpDeliveryS3Bucket = a})

-- | The prefix for the Amazon S3 bucket.
pocpDeliveryS3KeyPrefix :: Lens' PutOrganizationConformancePack (Maybe Text)
pocpDeliveryS3KeyPrefix = lens _pocpDeliveryS3KeyPrefix (\s a -> s {_pocpDeliveryS3KeyPrefix = a})

-- | Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
pocpTemplateS3URI :: Lens' PutOrganizationConformancePack (Maybe Text)
pocpTemplateS3URI = lens _pocpTemplateS3URI (\s a -> s {_pocpTemplateS3URI = a})

-- | A list of @ConformancePackInputParameter@ objects.
pocpConformancePackInputParameters :: Lens' PutOrganizationConformancePack [ConformancePackInputParameter]
pocpConformancePackInputParameters = lens _pocpConformancePackInputParameters (\s a -> s {_pocpConformancePackInputParameters = a}) . _Default . _Coerce

-- | A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
pocpExcludedAccounts :: Lens' PutOrganizationConformancePack [Text]
pocpExcludedAccounts = lens _pocpExcludedAccounts (\s a -> s {_pocpExcludedAccounts = a}) . _Default . _Coerce

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
pocpTemplateBody :: Lens' PutOrganizationConformancePack (Maybe Text)
pocpTemplateBody = lens _pocpTemplateBody (\s a -> s {_pocpTemplateBody = a})

-- | Name of the organization conformance pack you want to create.
pocpOrganizationConformancePackName :: Lens' PutOrganizationConformancePack Text
pocpOrganizationConformancePackName = lens _pocpOrganizationConformancePackName (\s a -> s {_pocpOrganizationConformancePackName = a})

instance AWSRequest PutOrganizationConformancePack where
  type
    Rs PutOrganizationConformancePack =
      PutOrganizationConformancePackResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          PutOrganizationConformancePackResponse'
            <$> (x .?> "OrganizationConformancePackArn") <*> (pure (fromEnum s))
      )

instance Hashable PutOrganizationConformancePack

instance NFData PutOrganizationConformancePack

instance ToHeaders PutOrganizationConformancePack where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.PutOrganizationConformancePack" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutOrganizationConformancePack where
  toJSON PutOrganizationConformancePack' {..} =
    object
      ( catMaybes
          [ ("DeliveryS3Bucket" .=) <$> _pocpDeliveryS3Bucket,
            ("DeliveryS3KeyPrefix" .=) <$> _pocpDeliveryS3KeyPrefix,
            ("TemplateS3Uri" .=) <$> _pocpTemplateS3URI,
            ("ConformancePackInputParameters" .=)
              <$> _pocpConformancePackInputParameters,
            ("ExcludedAccounts" .=) <$> _pocpExcludedAccounts,
            ("TemplateBody" .=) <$> _pocpTemplateBody,
            Just
              ( "OrganizationConformancePackName"
                  .= _pocpOrganizationConformancePackName
              )
          ]
      )

instance ToPath PutOrganizationConformancePack where
  toPath = const "/"

instance ToQuery PutOrganizationConformancePack where
  toQuery = const mempty

-- | /See:/ 'putOrganizationConformancePackResponse' smart constructor.
data PutOrganizationConformancePackResponse = PutOrganizationConformancePackResponse'
  { _pocprsOrganizationConformancePackARN ::
      !(Maybe Text),
    _pocprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutOrganizationConformancePackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocprsOrganizationConformancePackARN' - ARN of the organization conformance pack.
--
-- * 'pocprsResponseStatus' - -- | The response status code.
putOrganizationConformancePackResponse ::
  -- | 'pocprsResponseStatus'
  Int ->
  PutOrganizationConformancePackResponse
putOrganizationConformancePackResponse pResponseStatus_ =
  PutOrganizationConformancePackResponse'
    { _pocprsOrganizationConformancePackARN =
        Nothing,
      _pocprsResponseStatus = pResponseStatus_
    }

-- | ARN of the organization conformance pack.
pocprsOrganizationConformancePackARN :: Lens' PutOrganizationConformancePackResponse (Maybe Text)
pocprsOrganizationConformancePackARN = lens _pocprsOrganizationConformancePackARN (\s a -> s {_pocprsOrganizationConformancePackARN = a})

-- | -- | The response status code.
pocprsResponseStatus :: Lens' PutOrganizationConformancePackResponse Int
pocprsResponseStatus = lens _pocprsResponseStatus (\s a -> s {_pocprsResponseStatus = a})

instance NFData PutOrganizationConformancePackResponse
