{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
  ( SecretsManagerSecretResourceData (..),

    -- * Smart constructor
    mkSecretsManagerSecretResourceData,

    -- * Lenses
    smsrdARN,
    smsrdAdditionalStagingLabelsToDownload,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager. AWS IoT Greengrass stores a local, encrypted copy of the secret on the Greengrass core, where it can be securely accessed by connectors and Lambda functions.
--
-- /See:/ 'mkSecretsManagerSecretResourceData' smart constructor.
data SecretsManagerSecretResourceData = SecretsManagerSecretResourceData'
  { -- | The ARN of the Secrets Manager secret to make available on the core. The value of the secret's latest version (represented by the ''AWSCURRENT'' staging label) is included by default.
    arn :: Core.Maybe Core.Text,
    -- | Optional. The staging labels whose values you want to make available on the core, in addition to ''AWSCURRENT''.
    additionalStagingLabelsToDownload :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecretsManagerSecretResourceData' value with any optional fields omitted.
mkSecretsManagerSecretResourceData ::
  SecretsManagerSecretResourceData
mkSecretsManagerSecretResourceData =
  SecretsManagerSecretResourceData'
    { arn = Core.Nothing,
      additionalStagingLabelsToDownload = Core.Nothing
    }

-- | The ARN of the Secrets Manager secret to make available on the core. The value of the secret's latest version (represented by the ''AWSCURRENT'' staging label) is included by default.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsrdARN :: Lens.Lens' SecretsManagerSecretResourceData (Core.Maybe Core.Text)
smsrdARN = Lens.field @"arn"
{-# DEPRECATED smsrdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Optional. The staging labels whose values you want to make available on the core, in addition to ''AWSCURRENT''.
--
-- /Note:/ Consider using 'additionalStagingLabelsToDownload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsrdAdditionalStagingLabelsToDownload :: Lens.Lens' SecretsManagerSecretResourceData (Core.Maybe [Core.Text])
smsrdAdditionalStagingLabelsToDownload = Lens.field @"additionalStagingLabelsToDownload"
{-# DEPRECATED smsrdAdditionalStagingLabelsToDownload "Use generic-lens or generic-optics with 'additionalStagingLabelsToDownload' instead." #-}

instance Core.FromJSON SecretsManagerSecretResourceData where
  toJSON SecretsManagerSecretResourceData {..} =
    Core.object
      ( Core.catMaybes
          [ ("ARN" Core..=) Core.<$> arn,
            ("AdditionalStagingLabelsToDownload" Core..=)
              Core.<$> additionalStagingLabelsToDownload
          ]
      )

instance Core.FromJSON SecretsManagerSecretResourceData where
  parseJSON =
    Core.withObject "SecretsManagerSecretResourceData" Core.$
      \x ->
        SecretsManagerSecretResourceData'
          Core.<$> (x Core..:? "ARN")
          Core.<*> (x Core..:? "AdditionalStagingLabelsToDownload")
