-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionCode
  ( FunctionCode (..),

    -- * Smart constructor
    mkFunctionCode,

    -- * Lenses
    fcS3ObjectVersion,
    fcS3Key,
    fcZipFile,
    fcS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The code for the Lambda function. You can specify either an object in Amazon S3, or upload a deployment package directly.
--
-- /See:/ 'mkFunctionCode' smart constructor.
data FunctionCode = FunctionCode'
  { s3ObjectVersion ::
      Lude.Maybe Lude.Text,
    s3Key :: Lude.Maybe Lude.Text,
    zipFile :: Lude.Maybe (Lude.Sensitive Lude.Base64),
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionCode' with the minimum fields required to make a request.
--
-- * 's3Bucket' - An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
-- * 's3Key' - The Amazon S3 key of the deployment package.
-- * 's3ObjectVersion' - For versioned objects, the version of the deployment package object to use.
-- * 'zipFile' - The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mkFunctionCode ::
  FunctionCode
mkFunctionCode =
  FunctionCode'
    { s3ObjectVersion = Lude.Nothing,
      s3Key = Lude.Nothing,
      zipFile = Lude.Nothing,
      s3Bucket = Lude.Nothing
    }

-- | For versioned objects, the version of the deployment package object to use.
--
-- /Note:/ Consider using 's3ObjectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcS3ObjectVersion :: Lens.Lens' FunctionCode (Lude.Maybe Lude.Text)
fcS3ObjectVersion = Lens.lens (s3ObjectVersion :: FunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {s3ObjectVersion = a} :: FunctionCode)
{-# DEPRECATED fcS3ObjectVersion "Use generic-lens or generic-optics with 's3ObjectVersion' instead." #-}

-- | The Amazon S3 key of the deployment package.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcS3Key :: Lens.Lens' FunctionCode (Lude.Maybe Lude.Text)
fcS3Key = Lens.lens (s3Key :: FunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: FunctionCode)
{-# DEPRECATED fcS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcZipFile :: Lens.Lens' FunctionCode (Lude.Maybe (Lude.Sensitive Lude.Base64))
fcZipFile = Lens.lens (zipFile :: FunctionCode -> Lude.Maybe (Lude.Sensitive Lude.Base64)) (\s a -> s {zipFile = a} :: FunctionCode)
{-# DEPRECATED fcZipFile "Use generic-lens or generic-optics with 'zipFile' instead." #-}

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcS3Bucket :: Lens.Lens' FunctionCode (Lude.Maybe Lude.Text)
fcS3Bucket = Lens.lens (s3Bucket :: FunctionCode -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: FunctionCode)
{-# DEPRECATED fcS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.ToJSON FunctionCode where
  toJSON FunctionCode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3ObjectVersion" Lude..=) Lude.<$> s3ObjectVersion,
            ("S3Key" Lude..=) Lude.<$> s3Key,
            ("ZipFile" Lude..=) Lude.<$> zipFile,
            ("S3Bucket" Lude..=) Lude.<$> s3Bucket
          ]
      )
