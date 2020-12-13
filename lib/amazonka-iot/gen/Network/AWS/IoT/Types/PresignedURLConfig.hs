{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PresignedURLConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PresignedURLConfig
  ( PresignedURLConfig (..),

    -- * Smart constructor
    mkPresignedURLConfig,

    -- * Lenses
    pucExpiresInSec,
    pucRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration for pre-signed S3 URLs.
--
-- /See:/ 'mkPresignedURLConfig' smart constructor.
data PresignedURLConfig = PresignedURLConfig'
  { -- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
    expiresInSec :: Lude.Maybe Lude.Natural,
    -- | The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PresignedURLConfig' with the minimum fields required to make a request.
--
-- * 'expiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
-- * 'roleARN' - The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
mkPresignedURLConfig ::
  PresignedURLConfig
mkPresignedURLConfig =
  PresignedURLConfig'
    { expiresInSec = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
--
-- /Note:/ Consider using 'expiresInSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pucExpiresInSec :: Lens.Lens' PresignedURLConfig (Lude.Maybe Lude.Natural)
pucExpiresInSec = Lens.lens (expiresInSec :: PresignedURLConfig -> Lude.Maybe Lude.Natural) (\s a -> s {expiresInSec = a} :: PresignedURLConfig)
{-# DEPRECATED pucExpiresInSec "Use generic-lens or generic-optics with 'expiresInSec' instead." #-}

-- | The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pucRoleARN :: Lens.Lens' PresignedURLConfig (Lude.Maybe Lude.Text)
pucRoleARN = Lens.lens (roleARN :: PresignedURLConfig -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: PresignedURLConfig)
{-# DEPRECATED pucRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON PresignedURLConfig where
  parseJSON =
    Lude.withObject
      "PresignedURLConfig"
      ( \x ->
          PresignedURLConfig'
            Lude.<$> (x Lude..:? "expiresInSec") Lude.<*> (x Lude..:? "roleArn")
      )

instance Lude.ToJSON PresignedURLConfig where
  toJSON PresignedURLConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expiresInSec" Lude..=) Lude.<$> expiresInSec,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )
