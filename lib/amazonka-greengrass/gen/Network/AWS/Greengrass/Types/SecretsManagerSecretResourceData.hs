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
    smsrdAdditionalStagingLabelsToDownload,
    smsrdARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attributes that define a secret resource, which references a secret from AWS Secrets Manager. AWS IoT Greengrass stores a local, encrypted copy of the secret on the Greengrass core, where it can be securely accessed by connectors and Lambda functions.
--
-- /See:/ 'mkSecretsManagerSecretResourceData' smart constructor.
data SecretsManagerSecretResourceData = SecretsManagerSecretResourceData'
  { additionalStagingLabelsToDownload ::
      Lude.Maybe [Lude.Text],
    arn ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecretsManagerSecretResourceData' with the minimum fields required to make a request.
--
-- * 'additionalStagingLabelsToDownload' - Optional. The staging labels whose values you want to make available on the core, in addition to ''AWSCURRENT''.
-- * 'arn' - The ARN of the Secrets Manager secret to make available on the core. The value of the secret's latest version (represented by the ''AWSCURRENT'' staging label) is included by default.
mkSecretsManagerSecretResourceData ::
  SecretsManagerSecretResourceData
mkSecretsManagerSecretResourceData =
  SecretsManagerSecretResourceData'
    { additionalStagingLabelsToDownload =
        Lude.Nothing,
      arn = Lude.Nothing
    }

-- | Optional. The staging labels whose values you want to make available on the core, in addition to ''AWSCURRENT''.
--
-- /Note:/ Consider using 'additionalStagingLabelsToDownload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsrdAdditionalStagingLabelsToDownload :: Lens.Lens' SecretsManagerSecretResourceData (Lude.Maybe [Lude.Text])
smsrdAdditionalStagingLabelsToDownload = Lens.lens (additionalStagingLabelsToDownload :: SecretsManagerSecretResourceData -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalStagingLabelsToDownload = a} :: SecretsManagerSecretResourceData)
{-# DEPRECATED smsrdAdditionalStagingLabelsToDownload "Use generic-lens or generic-optics with 'additionalStagingLabelsToDownload' instead." #-}

-- | The ARN of the Secrets Manager secret to make available on the core. The value of the secret's latest version (represented by the ''AWSCURRENT'' staging label) is included by default.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsrdARN :: Lens.Lens' SecretsManagerSecretResourceData (Lude.Maybe Lude.Text)
smsrdARN = Lens.lens (arn :: SecretsManagerSecretResourceData -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SecretsManagerSecretResourceData)
{-# DEPRECATED smsrdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON SecretsManagerSecretResourceData where
  parseJSON =
    Lude.withObject
      "SecretsManagerSecretResourceData"
      ( \x ->
          SecretsManagerSecretResourceData'
            Lude.<$> ( x Lude..:? "AdditionalStagingLabelsToDownload"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "ARN")
      )

instance Lude.ToJSON SecretsManagerSecretResourceData where
  toJSON SecretsManagerSecretResourceData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AdditionalStagingLabelsToDownload" Lude..=)
              Lude.<$> additionalStagingLabelsToDownload,
            ("ARN" Lude..=) Lude.<$> arn
          ]
      )
