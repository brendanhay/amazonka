{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- Only a master account and a delegated administrator can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- This API enables organization service access for @config-multiaccountsetup.amazonaws.com@ through the @EnableAWSServiceAccess@ action and creates a service linked role @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated administrator account of your organization. The service linked role is created only when the role does not exist in the caller account. To use this API with delegated administrator, register a delegated administrator by calling AWS Organization @register-delegate-admin@ for @config-multiaccountsetup.amazonaws.com@ .
module Network.AWS.Config.PutOrganizationConformancePack
  ( -- * Creating a request
    PutOrganizationConformancePack (..),
    mkPutOrganizationConformancePack,

    -- ** Request lenses
    pocpDeliveryS3Bucket,
    pocpDeliveryS3KeyPrefix,
    pocpTemplateS3URI,
    pocpConformancePackInputParameters,
    pocpExcludedAccounts,
    pocpTemplateBody,
    pocpOrganizationConformancePackName,

    -- * Destructuring the response
    PutOrganizationConformancePackResponse (..),
    mkPutOrganizationConformancePackResponse,

    -- ** Response lenses
    pocprsOrganizationConformancePackARN,
    pocprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutOrganizationConformancePack' smart constructor.
data PutOrganizationConformancePack = PutOrganizationConformancePack'
  { deliveryS3Bucket ::
      Lude.Maybe Lude.Text,
    deliveryS3KeyPrefix ::
      Lude.Maybe Lude.Text,
    templateS3URI ::
      Lude.Maybe Lude.Text,
    conformancePackInputParameters ::
      Lude.Maybe
        [ConformancePackInputParameter],
    excludedAccounts ::
      Lude.Maybe [Lude.Text],
    templateBody ::
      Lude.Maybe Lude.Text,
    organizationConformancePackName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutOrganizationConformancePack' with the minimum fields required to make a request.
--
-- * 'conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
-- * 'deliveryS3Bucket' - Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template.
--
-- The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
-- * 'deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
-- * 'excludedAccounts' - A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
-- * 'organizationConformancePackName' - Name of the organization conformance pack you want to create.
-- * 'templateBody' - A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
-- * 'templateS3URI' - Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
mkPutOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Lude.Text ->
  PutOrganizationConformancePack
mkPutOrganizationConformancePack pOrganizationConformancePackName_ =
  PutOrganizationConformancePack'
    { deliveryS3Bucket = Lude.Nothing,
      deliveryS3KeyPrefix = Lude.Nothing,
      templateS3URI = Lude.Nothing,
      conformancePackInputParameters = Lude.Nothing,
      excludedAccounts = Lude.Nothing,
      templateBody = Lude.Nothing,
      organizationConformancePackName =
        pOrganizationConformancePackName_
    }

-- | Location of an Amazon S3 bucket where AWS Config can deliver evaluation results. AWS Config stores intermediate files while processing conformance pack template.
--
-- The delivery bucket name should start with awsconfigconforms. For example: "Resource": "arn:aws:s3:::your_bucket_name/*". For more information, see <https://docs.aws.amazon.com/config/latest/developerguide/conformance-pack-organization-apis.html Permissions for cross account bucket access> .
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpDeliveryS3Bucket :: Lens.Lens' PutOrganizationConformancePack (Lude.Maybe Lude.Text)
pocpDeliveryS3Bucket = Lens.lens (deliveryS3Bucket :: PutOrganizationConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3Bucket = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpDeliveryS3KeyPrefix :: Lens.Lens' PutOrganizationConformancePack (Lude.Maybe Lude.Text)
pocpDeliveryS3KeyPrefix = Lens.lens (deliveryS3KeyPrefix :: PutOrganizationConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3KeyPrefix = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | Location of file containing the template body. The uri must point to the conformance pack template (max size: 300 KB).
--
-- /Note:/ Consider using 'templateS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpTemplateS3URI :: Lens.Lens' PutOrganizationConformancePack (Lude.Maybe Lude.Text)
pocpTemplateS3URI = Lens.lens (templateS3URI :: PutOrganizationConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {templateS3URI = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpTemplateS3URI "Use generic-lens or generic-optics with 'templateS3URI' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpConformancePackInputParameters :: Lens.Lens' PutOrganizationConformancePack (Lude.Maybe [ConformancePackInputParameter])
pocpConformancePackInputParameters = Lens.lens (conformancePackInputParameters :: PutOrganizationConformancePack -> Lude.Maybe [ConformancePackInputParameter]) (\s a -> s {conformancePackInputParameters = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | A list of AWS accounts to be excluded from an organization conformance pack while deploying a conformance pack.
--
-- /Note:/ Consider using 'excludedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpExcludedAccounts :: Lens.Lens' PutOrganizationConformancePack (Lude.Maybe [Lude.Text])
pocpExcludedAccounts = Lens.lens (excludedAccounts :: PutOrganizationConformancePack -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedAccounts = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpExcludedAccounts "Use generic-lens or generic-optics with 'excludedAccounts' instead." #-}

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpTemplateBody :: Lens.Lens' PutOrganizationConformancePack (Lude.Maybe Lude.Text)
pocpTemplateBody = Lens.lens (templateBody :: PutOrganizationConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Name of the organization conformance pack you want to create.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocpOrganizationConformancePackName :: Lens.Lens' PutOrganizationConformancePack Lude.Text
pocpOrganizationConformancePackName = Lens.lens (organizationConformancePackName :: PutOrganizationConformancePack -> Lude.Text) (\s a -> s {organizationConformancePackName = a} :: PutOrganizationConformancePack)
{-# DEPRECATED pocpOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

instance Lude.AWSRequest PutOrganizationConformancePack where
  type
    Rs PutOrganizationConformancePack =
      PutOrganizationConformancePackResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutOrganizationConformancePackResponse'
            Lude.<$> (x Lude..?> "OrganizationConformancePackArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutOrganizationConformancePack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutOrganizationConformancePack" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutOrganizationConformancePack where
  toJSON PutOrganizationConformancePack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeliveryS3Bucket" Lude..=) Lude.<$> deliveryS3Bucket,
            ("DeliveryS3KeyPrefix" Lude..=) Lude.<$> deliveryS3KeyPrefix,
            ("TemplateS3Uri" Lude..=) Lude.<$> templateS3URI,
            ("ConformancePackInputParameters" Lude..=)
              Lude.<$> conformancePackInputParameters,
            ("ExcludedAccounts" Lude..=) Lude.<$> excludedAccounts,
            ("TemplateBody" Lude..=) Lude.<$> templateBody,
            Lude.Just
              ( "OrganizationConformancePackName"
                  Lude..= organizationConformancePackName
              )
          ]
      )

instance Lude.ToPath PutOrganizationConformancePack where
  toPath = Lude.const "/"

instance Lude.ToQuery PutOrganizationConformancePack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutOrganizationConformancePackResponse' smart constructor.
data PutOrganizationConformancePackResponse = PutOrganizationConformancePackResponse'
  { organizationConformancePackARN ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutOrganizationConformancePackResponse' with the minimum fields required to make a request.
--
-- * 'organizationConformancePackARN' - ARN of the organization conformance pack.
-- * 'responseStatus' - The response status code.
mkPutOrganizationConformancePackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutOrganizationConformancePackResponse
mkPutOrganizationConformancePackResponse pResponseStatus_ =
  PutOrganizationConformancePackResponse'
    { organizationConformancePackARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | ARN of the organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocprsOrganizationConformancePackARN :: Lens.Lens' PutOrganizationConformancePackResponse (Lude.Maybe Lude.Text)
pocprsOrganizationConformancePackARN = Lens.lens (organizationConformancePackARN :: PutOrganizationConformancePackResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationConformancePackARN = a} :: PutOrganizationConformancePackResponse)
{-# DEPRECATED pocprsOrganizationConformancePackARN "Use generic-lens or generic-optics with 'organizationConformancePackARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pocprsResponseStatus :: Lens.Lens' PutOrganizationConformancePackResponse Lude.Int
pocprsResponseStatus = Lens.lens (responseStatus :: PutOrganizationConformancePackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutOrganizationConformancePackResponse)
{-# DEPRECATED pocprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
