{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GenerateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an AWS CloudFormation template based on the current launch configuration and writes it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Network.AWS.SMS.GenerateTemplate
  ( -- * Creating a request
    GenerateTemplate (..),
    mkGenerateTemplate,

    -- ** Request lenses
    gtAppId,
    gtTemplateFormat,

    -- * Destructuring the response
    GenerateTemplateResponse (..),
    mkGenerateTemplateResponse,

    -- ** Response lenses
    gtrsS3Location,
    gtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGenerateTemplate' smart constructor.
data GenerateTemplate = GenerateTemplate'
  { -- | The ID of the application associated with the AWS CloudFormation template.
    appId :: Lude.Maybe Lude.Text,
    -- | The format for generating the AWS CloudFormation template.
    templateFormat :: Lude.Maybe OutputFormat
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateTemplate' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application associated with the AWS CloudFormation template.
-- * 'templateFormat' - The format for generating the AWS CloudFormation template.
mkGenerateTemplate ::
  GenerateTemplate
mkGenerateTemplate =
  GenerateTemplate'
    { appId = Lude.Nothing,
      templateFormat = Lude.Nothing
    }

-- | The ID of the application associated with the AWS CloudFormation template.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtAppId :: Lens.Lens' GenerateTemplate (Lude.Maybe Lude.Text)
gtAppId = Lens.lens (appId :: GenerateTemplate -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: GenerateTemplate)
{-# DEPRECATED gtAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The format for generating the AWS CloudFormation template.
--
-- /Note:/ Consider using 'templateFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTemplateFormat :: Lens.Lens' GenerateTemplate (Lude.Maybe OutputFormat)
gtTemplateFormat = Lens.lens (templateFormat :: GenerateTemplate -> Lude.Maybe OutputFormat) (\s a -> s {templateFormat = a} :: GenerateTemplate)
{-# DEPRECATED gtTemplateFormat "Use generic-lens or generic-optics with 'templateFormat' instead." #-}

instance Lude.AWSRequest GenerateTemplate where
  type Rs GenerateTemplate = GenerateTemplateResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GenerateTemplateResponse'
            Lude.<$> (x Lude..?> "s3Location") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GenerateTemplate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GenerateTemplate where
  toJSON GenerateTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("appId" Lude..=) Lude.<$> appId,
            ("templateFormat" Lude..=) Lude.<$> templateFormat
          ]
      )

instance Lude.ToPath GenerateTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGenerateTemplateResponse' smart constructor.
data GenerateTemplateResponse = GenerateTemplateResponse'
  { -- | The location of the Amazon S3 object.
    s3Location :: Lude.Maybe S3Location,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateTemplateResponse' with the minimum fields required to make a request.
--
-- * 's3Location' - The location of the Amazon S3 object.
-- * 'responseStatus' - The response status code.
mkGenerateTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateTemplateResponse
mkGenerateTemplateResponse pResponseStatus_ =
  GenerateTemplateResponse'
    { s3Location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The location of the Amazon S3 object.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsS3Location :: Lens.Lens' GenerateTemplateResponse (Lude.Maybe S3Location)
gtrsS3Location = Lens.lens (s3Location :: GenerateTemplateResponse -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: GenerateTemplateResponse)
{-# DEPRECATED gtrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GenerateTemplateResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GenerateTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateTemplateResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
