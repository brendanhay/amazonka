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
-- Module      : Amazonka.Signer.Types.SigningProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.Types.SignatureValidityPeriod
import Amazonka.Signer.Types.SigningMaterial
import Amazonka.Signer.Types.SigningProfileStatus

-- | Contains information about the ACM certificates and code signing
-- configuration parameters that can be used by a given code signing user.
--
-- /See:/ 'newSigningProfile' smart constructor.
data SigningProfile = SigningProfile'
  { -- | A list of tags associated with the signing profile.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The validity period for a signing job created using this signing
    -- profile.
    signatureValidityPeriod :: Prelude.Maybe SignatureValidityPeriod,
    -- | The name of the signing profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The parameters that are available for use by a code signing user.
    signingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of a signing profile, including the profile version.
    profileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the signing platform.
    platformDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the signing profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of a code signing profile.
    status :: Prelude.Maybe SigningProfileStatus,
    -- | The version of a signing profile.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The ACM certificate that is available for use by a signing profile.
    signingMaterial :: Prelude.Maybe SigningMaterial,
    -- | The ID of a platform that is available for use by a signing profile.
    platformId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'signingProfile_tags' - A list of tags associated with the signing profile.
--
-- 'signatureValidityPeriod', 'signingProfile_signatureValidityPeriod' - The validity period for a signing job created using this signing
-- profile.
--
-- 'profileName', 'signingProfile_profileName' - The name of the signing profile.
--
-- 'signingParameters', 'signingProfile_signingParameters' - The parameters that are available for use by a code signing user.
--
-- 'profileVersionArn', 'signingProfile_profileVersionArn' - The ARN of a signing profile, including the profile version.
--
-- 'platformDisplayName', 'signingProfile_platformDisplayName' - The name of the signing platform.
--
-- 'arn', 'signingProfile_arn' - The Amazon Resource Name (ARN) for the signing profile.
--
-- 'status', 'signingProfile_status' - The status of a code signing profile.
--
-- 'profileVersion', 'signingProfile_profileVersion' - The version of a signing profile.
--
-- 'signingMaterial', 'signingProfile_signingMaterial' - The ACM certificate that is available for use by a signing profile.
--
-- 'platformId', 'signingProfile_platformId' - The ID of a platform that is available for use by a signing profile.
newSigningProfile ::
  SigningProfile
newSigningProfile =
  SigningProfile'
    { tags = Prelude.Nothing,
      signatureValidityPeriod = Prelude.Nothing,
      profileName = Prelude.Nothing,
      signingParameters = Prelude.Nothing,
      profileVersionArn = Prelude.Nothing,
      platformDisplayName = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      signingMaterial = Prelude.Nothing,
      platformId = Prelude.Nothing
    }

-- | A list of tags associated with the signing profile.
signingProfile_tags :: Lens.Lens' SigningProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signingProfile_tags = Lens.lens (\SigningProfile' {tags} -> tags) (\s@SigningProfile' {} a -> s {tags = a} :: SigningProfile) Prelude.. Lens.mapping Lens.coerced

-- | The validity period for a signing job created using this signing
-- profile.
signingProfile_signatureValidityPeriod :: Lens.Lens' SigningProfile (Prelude.Maybe SignatureValidityPeriod)
signingProfile_signatureValidityPeriod = Lens.lens (\SigningProfile' {signatureValidityPeriod} -> signatureValidityPeriod) (\s@SigningProfile' {} a -> s {signatureValidityPeriod = a} :: SigningProfile)

-- | The name of the signing profile.
signingProfile_profileName :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_profileName = Lens.lens (\SigningProfile' {profileName} -> profileName) (\s@SigningProfile' {} a -> s {profileName = a} :: SigningProfile)

-- | The parameters that are available for use by a code signing user.
signingProfile_signingParameters :: Lens.Lens' SigningProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signingProfile_signingParameters = Lens.lens (\SigningProfile' {signingParameters} -> signingParameters) (\s@SigningProfile' {} a -> s {signingParameters = a} :: SigningProfile) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of a signing profile, including the profile version.
signingProfile_profileVersionArn :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_profileVersionArn = Lens.lens (\SigningProfile' {profileVersionArn} -> profileVersionArn) (\s@SigningProfile' {} a -> s {profileVersionArn = a} :: SigningProfile)

-- | The name of the signing platform.
signingProfile_platformDisplayName :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_platformDisplayName = Lens.lens (\SigningProfile' {platformDisplayName} -> platformDisplayName) (\s@SigningProfile' {} a -> s {platformDisplayName = a} :: SigningProfile)

-- | The Amazon Resource Name (ARN) for the signing profile.
signingProfile_arn :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_arn = Lens.lens (\SigningProfile' {arn} -> arn) (\s@SigningProfile' {} a -> s {arn = a} :: SigningProfile)

-- | The status of a code signing profile.
signingProfile_status :: Lens.Lens' SigningProfile (Prelude.Maybe SigningProfileStatus)
signingProfile_status = Lens.lens (\SigningProfile' {status} -> status) (\s@SigningProfile' {} a -> s {status = a} :: SigningProfile)

-- | The version of a signing profile.
signingProfile_profileVersion :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_profileVersion = Lens.lens (\SigningProfile' {profileVersion} -> profileVersion) (\s@SigningProfile' {} a -> s {profileVersion = a} :: SigningProfile)

-- | The ACM certificate that is available for use by a signing profile.
signingProfile_signingMaterial :: Lens.Lens' SigningProfile (Prelude.Maybe SigningMaterial)
signingProfile_signingMaterial = Lens.lens (\SigningProfile' {signingMaterial} -> signingMaterial) (\s@SigningProfile' {} a -> s {signingMaterial = a} :: SigningProfile)

-- | The ID of a platform that is available for use by a signing profile.
signingProfile_platformId :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_platformId = Lens.lens (\SigningProfile' {platformId} -> platformId) (\s@SigningProfile' {} a -> s {platformId = a} :: SigningProfile)

instance Data.FromJSON SigningProfile where
  parseJSON =
    Data.withObject
      "SigningProfile"
      ( \x ->
          SigningProfile'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "signatureValidityPeriod")
            Prelude.<*> (x Data..:? "profileName")
            Prelude.<*> ( x Data..:? "signingParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "profileVersionArn")
            Prelude.<*> (x Data..:? "platformDisplayName")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "profileVersion")
            Prelude.<*> (x Data..:? "signingMaterial")
            Prelude.<*> (x Data..:? "platformId")
      )

instance Prelude.Hashable SigningProfile where
  hashWithSalt _salt SigningProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` signatureValidityPeriod
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` signingParameters
      `Prelude.hashWithSalt` profileVersionArn
      `Prelude.hashWithSalt` platformDisplayName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` profileVersion
      `Prelude.hashWithSalt` signingMaterial
      `Prelude.hashWithSalt` platformId

instance Prelude.NFData SigningProfile where
  rnf SigningProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf signatureValidityPeriod
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf signingParameters
      `Prelude.seq` Prelude.rnf profileVersionArn
      `Prelude.seq` Prelude.rnf platformDisplayName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf signingMaterial
      `Prelude.seq` Prelude.rnf platformId
