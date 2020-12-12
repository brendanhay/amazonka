{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
  ( FlowDefinitionOutputConfig (..),

    -- * Smart constructor
    mkFlowDefinitionOutputConfig,

    -- * Lenses
    fdocKMSKeyId,
    fdocS3OutputPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about where human output will be stored.
--
-- /See:/ 'mkFlowDefinitionOutputConfig' smart constructor.
data FlowDefinitionOutputConfig = FlowDefinitionOutputConfig'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    s3OutputPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlowDefinitionOutputConfig' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side encryption.
-- * 's3OutputPath' - The Amazon S3 path where the object containing human output will be made available.
mkFlowDefinitionOutputConfig ::
  -- | 's3OutputPath'
  Lude.Text ->
  FlowDefinitionOutputConfig
mkFlowDefinitionOutputConfig pS3OutputPath_ =
  FlowDefinitionOutputConfig'
    { kmsKeyId = Lude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdocKMSKeyId :: Lens.Lens' FlowDefinitionOutputConfig (Lude.Maybe Lude.Text)
fdocKMSKeyId = Lens.lens (kmsKeyId :: FlowDefinitionOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: FlowDefinitionOutputConfig)
{-# DEPRECATED fdocKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Amazon S3 path where the object containing human output will be made available.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdocS3OutputPath :: Lens.Lens' FlowDefinitionOutputConfig Lude.Text
fdocS3OutputPath = Lens.lens (s3OutputPath :: FlowDefinitionOutputConfig -> Lude.Text) (\s a -> s {s3OutputPath = a} :: FlowDefinitionOutputConfig)
{-# DEPRECATED fdocS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

instance Lude.FromJSON FlowDefinitionOutputConfig where
  parseJSON =
    Lude.withObject
      "FlowDefinitionOutputConfig"
      ( \x ->
          FlowDefinitionOutputConfig'
            Lude.<$> (x Lude..:? "KmsKeyId") Lude.<*> (x Lude..: "S3OutputPath")
      )

instance Lude.ToJSON FlowDefinitionOutputConfig where
  toJSON FlowDefinitionOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            Lude.Just ("S3OutputPath" Lude..= s3OutputPath)
          ]
      )
