{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SigningProfileParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SigningProfileParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the code-signing profile.
--
-- /See:/ 'newSigningProfileParameter' smart constructor.
data SigningProfileParameter = SigningProfileParameter'
  { -- | The hardware platform of your device.
    platform :: Core.Maybe Core.Text,
    -- | Certificate ARN.
    certificateArn :: Core.Maybe Core.Text,
    -- | The location of the code-signing certificate on your device.
    certificatePathOnDevice :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SigningProfileParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'signingProfileParameter_platform' - The hardware platform of your device.
--
-- 'certificateArn', 'signingProfileParameter_certificateArn' - Certificate ARN.
--
-- 'certificatePathOnDevice', 'signingProfileParameter_certificatePathOnDevice' - The location of the code-signing certificate on your device.
newSigningProfileParameter ::
  SigningProfileParameter
newSigningProfileParameter =
  SigningProfileParameter'
    { platform = Core.Nothing,
      certificateArn = Core.Nothing,
      certificatePathOnDevice = Core.Nothing
    }

-- | The hardware platform of your device.
signingProfileParameter_platform :: Lens.Lens' SigningProfileParameter (Core.Maybe Core.Text)
signingProfileParameter_platform = Lens.lens (\SigningProfileParameter' {platform} -> platform) (\s@SigningProfileParameter' {} a -> s {platform = a} :: SigningProfileParameter)

-- | Certificate ARN.
signingProfileParameter_certificateArn :: Lens.Lens' SigningProfileParameter (Core.Maybe Core.Text)
signingProfileParameter_certificateArn = Lens.lens (\SigningProfileParameter' {certificateArn} -> certificateArn) (\s@SigningProfileParameter' {} a -> s {certificateArn = a} :: SigningProfileParameter)

-- | The location of the code-signing certificate on your device.
signingProfileParameter_certificatePathOnDevice :: Lens.Lens' SigningProfileParameter (Core.Maybe Core.Text)
signingProfileParameter_certificatePathOnDevice = Lens.lens (\SigningProfileParameter' {certificatePathOnDevice} -> certificatePathOnDevice) (\s@SigningProfileParameter' {} a -> s {certificatePathOnDevice = a} :: SigningProfileParameter)

instance Core.FromJSON SigningProfileParameter where
  parseJSON =
    Core.withObject
      "SigningProfileParameter"
      ( \x ->
          SigningProfileParameter'
            Core.<$> (x Core..:? "platform")
            Core.<*> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "certificatePathOnDevice")
      )

instance Core.Hashable SigningProfileParameter

instance Core.NFData SigningProfileParameter

instance Core.ToJSON SigningProfileParameter where
  toJSON SigningProfileParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("platform" Core..=) Core.<$> platform,
            ("certificateArn" Core..=) Core.<$> certificateArn,
            ("certificatePathOnDevice" Core..=)
              Core.<$> certificatePathOnDevice
          ]
      )
