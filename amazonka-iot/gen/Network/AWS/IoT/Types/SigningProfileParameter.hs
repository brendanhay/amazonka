{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the code-signing profile.
--
-- /See:/ 'newSigningProfileParameter' smart constructor.
data SigningProfileParameter = SigningProfileParameter'
  { -- | The hardware platform of your device.
    platform :: Prelude.Maybe Prelude.Text,
    -- | Certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The location of the code-signing certificate on your device.
    certificatePathOnDevice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { platform =
        Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificatePathOnDevice = Prelude.Nothing
    }

-- | The hardware platform of your device.
signingProfileParameter_platform :: Lens.Lens' SigningProfileParameter (Prelude.Maybe Prelude.Text)
signingProfileParameter_platform = Lens.lens (\SigningProfileParameter' {platform} -> platform) (\s@SigningProfileParameter' {} a -> s {platform = a} :: SigningProfileParameter)

-- | Certificate ARN.
signingProfileParameter_certificateArn :: Lens.Lens' SigningProfileParameter (Prelude.Maybe Prelude.Text)
signingProfileParameter_certificateArn = Lens.lens (\SigningProfileParameter' {certificateArn} -> certificateArn) (\s@SigningProfileParameter' {} a -> s {certificateArn = a} :: SigningProfileParameter)

-- | The location of the code-signing certificate on your device.
signingProfileParameter_certificatePathOnDevice :: Lens.Lens' SigningProfileParameter (Prelude.Maybe Prelude.Text)
signingProfileParameter_certificatePathOnDevice = Lens.lens (\SigningProfileParameter' {certificatePathOnDevice} -> certificatePathOnDevice) (\s@SigningProfileParameter' {} a -> s {certificatePathOnDevice = a} :: SigningProfileParameter)

instance Prelude.FromJSON SigningProfileParameter where
  parseJSON =
    Prelude.withObject
      "SigningProfileParameter"
      ( \x ->
          SigningProfileParameter'
            Prelude.<$> (x Prelude..:? "platform")
            Prelude.<*> (x Prelude..:? "certificateArn")
            Prelude.<*> (x Prelude..:? "certificatePathOnDevice")
      )

instance Prelude.Hashable SigningProfileParameter

instance Prelude.NFData SigningProfileParameter

instance Prelude.ToJSON SigningProfileParameter where
  toJSON SigningProfileParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("platform" Prelude..=) Prelude.<$> platform,
            ("certificateArn" Prelude..=)
              Prelude.<$> certificateArn,
            ("certificatePathOnDevice" Prelude..=)
              Prelude.<$> certificatePathOnDevice
          ]
      )
