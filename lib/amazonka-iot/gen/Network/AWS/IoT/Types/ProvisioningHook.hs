{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningHook
  ( ProvisioningHook (..),

    -- * Smart constructor
    mkProvisioningHook,

    -- * Lenses
    phTargetARN,
    phPayloadVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Structure that contains @payloadVersion@ and @targetArn@ .
--
-- /See:/ 'mkProvisioningHook' smart constructor.
data ProvisioningHook = ProvisioningHook'
  { -- | The ARN of the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    targetARN :: Lude.Text,
    -- | The payload that was sent to the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    payloadVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningHook' with the minimum fields required to make a request.
--
-- * 'targetARN' - The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
-- * 'payloadVersion' - The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
mkProvisioningHook ::
  -- | 'targetARN'
  Lude.Text ->
  ProvisioningHook
mkProvisioningHook pTargetARN_ =
  ProvisioningHook'
    { targetARN = pTargetARN_,
      payloadVersion = Lude.Nothing
    }

-- | The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phTargetARN :: Lens.Lens' ProvisioningHook Lude.Text
phTargetARN = Lens.lens (targetARN :: ProvisioningHook -> Lude.Text) (\s a -> s {targetARN = a} :: ProvisioningHook)
{-# DEPRECATED phTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
--
-- /Note:/ Consider using 'payloadVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phPayloadVersion :: Lens.Lens' ProvisioningHook (Lude.Maybe Lude.Text)
phPayloadVersion = Lens.lens (payloadVersion :: ProvisioningHook -> Lude.Maybe Lude.Text) (\s a -> s {payloadVersion = a} :: ProvisioningHook)
{-# DEPRECATED phPayloadVersion "Use generic-lens or generic-optics with 'payloadVersion' instead." #-}

instance Lude.FromJSON ProvisioningHook where
  parseJSON =
    Lude.withObject
      "ProvisioningHook"
      ( \x ->
          ProvisioningHook'
            Lude.<$> (x Lude..: "targetArn") Lude.<*> (x Lude..:? "payloadVersion")
      )

instance Lude.ToJSON ProvisioningHook where
  toJSON ProvisioningHook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("targetArn" Lude..= targetARN),
            ("payloadVersion" Lude..=) Lude.<$> payloadVersion
          ]
      )
