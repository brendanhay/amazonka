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
-- Module      : Amazonka.IoT.Types.SigningProfileParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.SigningProfileParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the code-signing profile.
--
-- /See:/ 'newSigningProfileParameter' smart constructor.
data SigningProfileParameter = SigningProfileParameter'
  { -- | Certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The location of the code-signing certificate on your device.
    certificatePathOnDevice :: Prelude.Maybe Prelude.Text,
    -- | The hardware platform of your device.
    platform :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningProfileParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'signingProfileParameter_certificateArn' - Certificate ARN.
--
-- 'certificatePathOnDevice', 'signingProfileParameter_certificatePathOnDevice' - The location of the code-signing certificate on your device.
--
-- 'platform', 'signingProfileParameter_platform' - The hardware platform of your device.
newSigningProfileParameter ::
  SigningProfileParameter
newSigningProfileParameter =
  SigningProfileParameter'
    { certificateArn =
        Prelude.Nothing,
      certificatePathOnDevice = Prelude.Nothing,
      platform = Prelude.Nothing
    }

-- | Certificate ARN.
signingProfileParameter_certificateArn :: Lens.Lens' SigningProfileParameter (Prelude.Maybe Prelude.Text)
signingProfileParameter_certificateArn = Lens.lens (\SigningProfileParameter' {certificateArn} -> certificateArn) (\s@SigningProfileParameter' {} a -> s {certificateArn = a} :: SigningProfileParameter)

-- | The location of the code-signing certificate on your device.
signingProfileParameter_certificatePathOnDevice :: Lens.Lens' SigningProfileParameter (Prelude.Maybe Prelude.Text)
signingProfileParameter_certificatePathOnDevice = Lens.lens (\SigningProfileParameter' {certificatePathOnDevice} -> certificatePathOnDevice) (\s@SigningProfileParameter' {} a -> s {certificatePathOnDevice = a} :: SigningProfileParameter)

-- | The hardware platform of your device.
signingProfileParameter_platform :: Lens.Lens' SigningProfileParameter (Prelude.Maybe Prelude.Text)
signingProfileParameter_platform = Lens.lens (\SigningProfileParameter' {platform} -> platform) (\s@SigningProfileParameter' {} a -> s {platform = a} :: SigningProfileParameter)

instance Data.FromJSON SigningProfileParameter where
  parseJSON =
    Data.withObject
      "SigningProfileParameter"
      ( \x ->
          SigningProfileParameter'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "certificatePathOnDevice")
            Prelude.<*> (x Data..:? "platform")
      )

instance Prelude.Hashable SigningProfileParameter where
  hashWithSalt _salt SigningProfileParameter' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificatePathOnDevice
      `Prelude.hashWithSalt` platform

instance Prelude.NFData SigningProfileParameter where
  rnf SigningProfileParameter' {..} =
    Prelude.rnf certificateArn `Prelude.seq`
      Prelude.rnf certificatePathOnDevice `Prelude.seq`
        Prelude.rnf platform

instance Data.ToJSON SigningProfileParameter where
  toJSON SigningProfileParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("certificatePathOnDevice" Data..=)
              Prelude.<$> certificatePathOnDevice,
            ("platform" Data..=) Prelude.<$> platform
          ]
      )
