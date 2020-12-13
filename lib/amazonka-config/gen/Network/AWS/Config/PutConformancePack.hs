{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a conformance pack. A conformance pack is a collection of AWS Config rules that can be easily deployed in an account and a region and across AWS Organization.
--
-- This API creates a service linked role @AWSServiceRoleForConfigConforms@ in your account. The service linked role is created only when the role does not exist in your account.
module Network.AWS.Config.PutConformancePack
  ( -- * Creating a request
    PutConformancePack (..),
    mkPutConformancePack,

    -- ** Request lenses
    pcpDeliveryS3Bucket,
    pcpConformancePackName,
    pcpDeliveryS3KeyPrefix,
    pcpTemplateS3URI,
    pcpConformancePackInputParameters,
    pcpTemplateBody,

    -- * Destructuring the response
    PutConformancePackResponse (..),
    mkPutConformancePackResponse,

    -- ** Response lenses
    pcprsConformancePackARN,
    pcprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutConformancePack' smart constructor.
data PutConformancePack = PutConformancePack'
  { -- | AWS Config stores intermediate files while processing conformance pack template.
    deliveryS3Bucket :: Lude.Maybe Lude.Text,
    -- | Name of the conformance pack you want to create.
    conformancePackName :: Lude.Text,
    -- | The prefix for the Amazon S3 bucket.
    deliveryS3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack.
    templateS3URI :: Lude.Maybe Lude.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Lude.Maybe [ConformancePackInputParameter],
    -- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
    templateBody :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConformancePack' with the minimum fields required to make a request.
--
-- * 'deliveryS3Bucket' - AWS Config stores intermediate files while processing conformance pack template.
-- * 'conformancePackName' - Name of the conformance pack you want to create.
-- * 'deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
-- * 'templateS3URI' - Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack.
-- * 'conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
-- * 'templateBody' - A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
mkPutConformancePack ::
  -- | 'conformancePackName'
  Lude.Text ->
  PutConformancePack
mkPutConformancePack pConformancePackName_ =
  PutConformancePack'
    { deliveryS3Bucket = Lude.Nothing,
      conformancePackName = pConformancePackName_,
      deliveryS3KeyPrefix = Lude.Nothing,
      templateS3URI = Lude.Nothing,
      conformancePackInputParameters = Lude.Nothing,
      templateBody = Lude.Nothing
    }

-- | AWS Config stores intermediate files while processing conformance pack template.
--
-- /Note:/ Consider using 'deliveryS3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpDeliveryS3Bucket :: Lens.Lens' PutConformancePack (Lude.Maybe Lude.Text)
pcpDeliveryS3Bucket = Lens.lens (deliveryS3Bucket :: PutConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3Bucket = a} :: PutConformancePack)
{-# DEPRECATED pcpDeliveryS3Bucket "Use generic-lens or generic-optics with 'deliveryS3Bucket' instead." #-}

-- | Name of the conformance pack you want to create.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConformancePackName :: Lens.Lens' PutConformancePack Lude.Text
pcpConformancePackName = Lens.lens (conformancePackName :: PutConformancePack -> Lude.Text) (\s a -> s {conformancePackName = a} :: PutConformancePack)
{-# DEPRECATED pcpConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | The prefix for the Amazon S3 bucket.
--
-- /Note:/ Consider using 'deliveryS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpDeliveryS3KeyPrefix :: Lens.Lens' PutConformancePack (Lude.Maybe Lude.Text)
pcpDeliveryS3KeyPrefix = Lens.lens (deliveryS3KeyPrefix :: PutConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {deliveryS3KeyPrefix = a} :: PutConformancePack)
{-# DEPRECATED pcpDeliveryS3KeyPrefix "Use generic-lens or generic-optics with 'deliveryS3KeyPrefix' instead." #-}

-- | Location of file containing the template body (@s3://bucketname/prefix@ ). The uri must point to the conformance pack template (max size: 300 KB) that is located in an Amazon S3 bucket in the same region as the conformance pack.
--
-- /Note:/ Consider using 'templateS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpTemplateS3URI :: Lens.Lens' PutConformancePack (Lude.Maybe Lude.Text)
pcpTemplateS3URI = Lens.lens (templateS3URI :: PutConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {templateS3URI = a} :: PutConformancePack)
{-# DEPRECATED pcpTemplateS3URI "Use generic-lens or generic-optics with 'templateS3URI' instead." #-}

-- | A list of @ConformancePackInputParameter@ objects.
--
-- /Note:/ Consider using 'conformancePackInputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConformancePackInputParameters :: Lens.Lens' PutConformancePack (Lude.Maybe [ConformancePackInputParameter])
pcpConformancePackInputParameters = Lens.lens (conformancePackInputParameters :: PutConformancePack -> Lude.Maybe [ConformancePackInputParameter]) (\s a -> s {conformancePackInputParameters = a} :: PutConformancePack)
{-# DEPRECATED pcpConformancePackInputParameters "Use generic-lens or generic-optics with 'conformancePackInputParameters' instead." #-}

-- | A string containing full conformance pack template body. Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpTemplateBody :: Lens.Lens' PutConformancePack (Lude.Maybe Lude.Text)
pcpTemplateBody = Lens.lens (templateBody :: PutConformancePack -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: PutConformancePack)
{-# DEPRECATED pcpTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

instance Lude.AWSRequest PutConformancePack where
  type Rs PutConformancePack = PutConformancePackResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutConformancePackResponse'
            Lude.<$> (x Lude..?> "ConformancePackArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutConformancePack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.PutConformancePack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutConformancePack where
  toJSON PutConformancePack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeliveryS3Bucket" Lude..=) Lude.<$> deliveryS3Bucket,
            Lude.Just ("ConformancePackName" Lude..= conformancePackName),
            ("DeliveryS3KeyPrefix" Lude..=) Lude.<$> deliveryS3KeyPrefix,
            ("TemplateS3Uri" Lude..=) Lude.<$> templateS3URI,
            ("ConformancePackInputParameters" Lude..=)
              Lude.<$> conformancePackInputParameters,
            ("TemplateBody" Lude..=) Lude.<$> templateBody
          ]
      )

instance Lude.ToPath PutConformancePack where
  toPath = Lude.const "/"

instance Lude.ToQuery PutConformancePack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutConformancePackResponse' smart constructor.
data PutConformancePackResponse = PutConformancePackResponse'
  { -- | ARN of the conformance pack.
    conformancePackARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConformancePackResponse' with the minimum fields required to make a request.
--
-- * 'conformancePackARN' - ARN of the conformance pack.
-- * 'responseStatus' - The response status code.
mkPutConformancePackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutConformancePackResponse
mkPutConformancePackResponse pResponseStatus_ =
  PutConformancePackResponse'
    { conformancePackARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | ARN of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprsConformancePackARN :: Lens.Lens' PutConformancePackResponse (Lude.Maybe Lude.Text)
pcprsConformancePackARN = Lens.lens (conformancePackARN :: PutConformancePackResponse -> Lude.Maybe Lude.Text) (\s a -> s {conformancePackARN = a} :: PutConformancePackResponse)
{-# DEPRECATED pcprsConformancePackARN "Use generic-lens or generic-optics with 'conformancePackARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprsResponseStatus :: Lens.Lens' PutConformancePackResponse Lude.Int
pcprsResponseStatus = Lens.lens (responseStatus :: PutConformancePackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutConformancePackResponse)
{-# DEPRECATED pcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
