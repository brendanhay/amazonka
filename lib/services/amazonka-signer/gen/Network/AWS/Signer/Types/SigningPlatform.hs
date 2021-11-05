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
-- Module      : Amazonka.Signer.Types.SigningPlatform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningPlatform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.Category
import Amazonka.Signer.Types.SigningConfiguration
import Amazonka.Signer.Types.SigningImageFormat

-- | Contains information about the signing configurations and parameters
-- that are used to perform a code signing job.
--
-- /See:/ 'newSigningPlatform' smart constructor.
data SigningPlatform = SigningPlatform'
  { -- | The category of a code signing platform.
    category :: Prelude.Maybe Category,
    -- | The configuration of a code signing platform. This includes the
    -- designated hash algorithm and encryption algorithm of a signing
    -- platform.
    signingConfiguration :: Prelude.Maybe SigningConfiguration,
    -- | Any partner entities linked to a code signing platform.
    partner :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether revocation is supported for the platform.
    revocationSupported :: Prelude.Maybe Prelude.Bool,
    signingImageFormat :: Prelude.Maybe SigningImageFormat,
    -- | The ID of a code signing; platform.
    platformId :: Prelude.Maybe Prelude.Text,
    -- | The display name of a code signing platform.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The maximum size (in MB) of code that can be signed by a code signing
    -- platform.
    maxSizeInMB :: Prelude.Maybe Prelude.Int,
    -- | The types of targets that can be signed by a code signing platform.
    target :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningPlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'signingPlatform_category' - The category of a code signing platform.
--
-- 'signingConfiguration', 'signingPlatform_signingConfiguration' - The configuration of a code signing platform. This includes the
-- designated hash algorithm and encryption algorithm of a signing
-- platform.
--
-- 'partner', 'signingPlatform_partner' - Any partner entities linked to a code signing platform.
--
-- 'revocationSupported', 'signingPlatform_revocationSupported' - Indicates whether revocation is supported for the platform.
--
-- 'signingImageFormat', 'signingPlatform_signingImageFormat' - Undocumented member.
--
-- 'platformId', 'signingPlatform_platformId' - The ID of a code signing; platform.
--
-- 'displayName', 'signingPlatform_displayName' - The display name of a code signing platform.
--
-- 'maxSizeInMB', 'signingPlatform_maxSizeInMB' - The maximum size (in MB) of code that can be signed by a code signing
-- platform.
--
-- 'target', 'signingPlatform_target' - The types of targets that can be signed by a code signing platform.
newSigningPlatform ::
  SigningPlatform
newSigningPlatform =
  SigningPlatform'
    { category = Prelude.Nothing,
      signingConfiguration = Prelude.Nothing,
      partner = Prelude.Nothing,
      revocationSupported = Prelude.Nothing,
      signingImageFormat = Prelude.Nothing,
      platformId = Prelude.Nothing,
      displayName = Prelude.Nothing,
      maxSizeInMB = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The category of a code signing platform.
signingPlatform_category :: Lens.Lens' SigningPlatform (Prelude.Maybe Category)
signingPlatform_category = Lens.lens (\SigningPlatform' {category} -> category) (\s@SigningPlatform' {} a -> s {category = a} :: SigningPlatform)

-- | The configuration of a code signing platform. This includes the
-- designated hash algorithm and encryption algorithm of a signing
-- platform.
signingPlatform_signingConfiguration :: Lens.Lens' SigningPlatform (Prelude.Maybe SigningConfiguration)
signingPlatform_signingConfiguration = Lens.lens (\SigningPlatform' {signingConfiguration} -> signingConfiguration) (\s@SigningPlatform' {} a -> s {signingConfiguration = a} :: SigningPlatform)

-- | Any partner entities linked to a code signing platform.
signingPlatform_partner :: Lens.Lens' SigningPlatform (Prelude.Maybe Prelude.Text)
signingPlatform_partner = Lens.lens (\SigningPlatform' {partner} -> partner) (\s@SigningPlatform' {} a -> s {partner = a} :: SigningPlatform)

-- | Indicates whether revocation is supported for the platform.
signingPlatform_revocationSupported :: Lens.Lens' SigningPlatform (Prelude.Maybe Prelude.Bool)
signingPlatform_revocationSupported = Lens.lens (\SigningPlatform' {revocationSupported} -> revocationSupported) (\s@SigningPlatform' {} a -> s {revocationSupported = a} :: SigningPlatform)

-- | Undocumented member.
signingPlatform_signingImageFormat :: Lens.Lens' SigningPlatform (Prelude.Maybe SigningImageFormat)
signingPlatform_signingImageFormat = Lens.lens (\SigningPlatform' {signingImageFormat} -> signingImageFormat) (\s@SigningPlatform' {} a -> s {signingImageFormat = a} :: SigningPlatform)

-- | The ID of a code signing; platform.
signingPlatform_platformId :: Lens.Lens' SigningPlatform (Prelude.Maybe Prelude.Text)
signingPlatform_platformId = Lens.lens (\SigningPlatform' {platformId} -> platformId) (\s@SigningPlatform' {} a -> s {platformId = a} :: SigningPlatform)

-- | The display name of a code signing platform.
signingPlatform_displayName :: Lens.Lens' SigningPlatform (Prelude.Maybe Prelude.Text)
signingPlatform_displayName = Lens.lens (\SigningPlatform' {displayName} -> displayName) (\s@SigningPlatform' {} a -> s {displayName = a} :: SigningPlatform)

-- | The maximum size (in MB) of code that can be signed by a code signing
-- platform.
signingPlatform_maxSizeInMB :: Lens.Lens' SigningPlatform (Prelude.Maybe Prelude.Int)
signingPlatform_maxSizeInMB = Lens.lens (\SigningPlatform' {maxSizeInMB} -> maxSizeInMB) (\s@SigningPlatform' {} a -> s {maxSizeInMB = a} :: SigningPlatform)

-- | The types of targets that can be signed by a code signing platform.
signingPlatform_target :: Lens.Lens' SigningPlatform (Prelude.Maybe Prelude.Text)
signingPlatform_target = Lens.lens (\SigningPlatform' {target} -> target) (\s@SigningPlatform' {} a -> s {target = a} :: SigningPlatform)

instance Core.FromJSON SigningPlatform where
  parseJSON =
    Core.withObject
      "SigningPlatform"
      ( \x ->
          SigningPlatform'
            Prelude.<$> (x Core..:? "category")
            Prelude.<*> (x Core..:? "signingConfiguration")
            Prelude.<*> (x Core..:? "partner")
            Prelude.<*> (x Core..:? "revocationSupported")
            Prelude.<*> (x Core..:? "signingImageFormat")
            Prelude.<*> (x Core..:? "platformId")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "maxSizeInMB")
            Prelude.<*> (x Core..:? "target")
      )

instance Prelude.Hashable SigningPlatform

instance Prelude.NFData SigningPlatform
