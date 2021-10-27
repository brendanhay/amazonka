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
-- Module      : Network.AWS.Signer.Types.SigningProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Signer.Types.SigningProfile where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Signer.Types.SignatureValidityPeriod
import Network.AWS.Signer.Types.SigningMaterial
import Network.AWS.Signer.Types.SigningProfileStatus

-- | Contains information about the ACM certificates and code signing
-- configuration parameters that can be used by a given code signing user.
--
-- /See:/ 'newSigningProfile' smart constructor.
data SigningProfile = SigningProfile'
  { -- | The status of a code signing profile.
    status :: Prelude.Maybe SigningProfileStatus,
    -- | The name of the signing platform.
    platformDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the signing profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ACM certificate that is available for use by a signing profile.
    signingMaterial :: Prelude.Maybe SigningMaterial,
    -- | The version of a signing profile.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the signing profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a signing profile, including the profile version.
    profileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a platform that is available for use by a signing profile.
    platformId :: Prelude.Maybe Prelude.Text,
    -- | The validity period for a signing job created using this signing
    -- profile.
    signatureValidityPeriod :: Prelude.Maybe SignatureValidityPeriod,
    -- | The parameters that are available for use by a code signing user.
    signingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of tags associated with the signing profile.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'status', 'signingProfile_status' - The status of a code signing profile.
--
-- 'platformDisplayName', 'signingProfile_platformDisplayName' - The name of the signing platform.
--
-- 'arn', 'signingProfile_arn' - The Amazon Resource Name (ARN) for the signing profile.
--
-- 'signingMaterial', 'signingProfile_signingMaterial' - The ACM certificate that is available for use by a signing profile.
--
-- 'profileVersion', 'signingProfile_profileVersion' - The version of a signing profile.
--
-- 'profileName', 'signingProfile_profileName' - The name of the signing profile.
--
-- 'profileVersionArn', 'signingProfile_profileVersionArn' - The ARN of a signing profile, including the profile version.
--
-- 'platformId', 'signingProfile_platformId' - The ID of a platform that is available for use by a signing profile.
--
-- 'signatureValidityPeriod', 'signingProfile_signatureValidityPeriod' - The validity period for a signing job created using this signing
-- profile.
--
-- 'signingParameters', 'signingProfile_signingParameters' - The parameters that are available for use by a code signing user.
--
-- 'tags', 'signingProfile_tags' - A list of tags associated with the signing profile.
newSigningProfile ::
  SigningProfile
newSigningProfile =
  SigningProfile'
    { status = Prelude.Nothing,
      platformDisplayName = Prelude.Nothing,
      arn = Prelude.Nothing,
      signingMaterial = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      profileName = Prelude.Nothing,
      profileVersionArn = Prelude.Nothing,
      platformId = Prelude.Nothing,
      signatureValidityPeriod = Prelude.Nothing,
      signingParameters = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of a code signing profile.
signingProfile_status :: Lens.Lens' SigningProfile (Prelude.Maybe SigningProfileStatus)
signingProfile_status = Lens.lens (\SigningProfile' {status} -> status) (\s@SigningProfile' {} a -> s {status = a} :: SigningProfile)

-- | The name of the signing platform.
signingProfile_platformDisplayName :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_platformDisplayName = Lens.lens (\SigningProfile' {platformDisplayName} -> platformDisplayName) (\s@SigningProfile' {} a -> s {platformDisplayName = a} :: SigningProfile)

-- | The Amazon Resource Name (ARN) for the signing profile.
signingProfile_arn :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_arn = Lens.lens (\SigningProfile' {arn} -> arn) (\s@SigningProfile' {} a -> s {arn = a} :: SigningProfile)

-- | The ACM certificate that is available for use by a signing profile.
signingProfile_signingMaterial :: Lens.Lens' SigningProfile (Prelude.Maybe SigningMaterial)
signingProfile_signingMaterial = Lens.lens (\SigningProfile' {signingMaterial} -> signingMaterial) (\s@SigningProfile' {} a -> s {signingMaterial = a} :: SigningProfile)

-- | The version of a signing profile.
signingProfile_profileVersion :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_profileVersion = Lens.lens (\SigningProfile' {profileVersion} -> profileVersion) (\s@SigningProfile' {} a -> s {profileVersion = a} :: SigningProfile)

-- | The name of the signing profile.
signingProfile_profileName :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_profileName = Lens.lens (\SigningProfile' {profileName} -> profileName) (\s@SigningProfile' {} a -> s {profileName = a} :: SigningProfile)

-- | The ARN of a signing profile, including the profile version.
signingProfile_profileVersionArn :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_profileVersionArn = Lens.lens (\SigningProfile' {profileVersionArn} -> profileVersionArn) (\s@SigningProfile' {} a -> s {profileVersionArn = a} :: SigningProfile)

-- | The ID of a platform that is available for use by a signing profile.
signingProfile_platformId :: Lens.Lens' SigningProfile (Prelude.Maybe Prelude.Text)
signingProfile_platformId = Lens.lens (\SigningProfile' {platformId} -> platformId) (\s@SigningProfile' {} a -> s {platformId = a} :: SigningProfile)

-- | The validity period for a signing job created using this signing
-- profile.
signingProfile_signatureValidityPeriod :: Lens.Lens' SigningProfile (Prelude.Maybe SignatureValidityPeriod)
signingProfile_signatureValidityPeriod = Lens.lens (\SigningProfile' {signatureValidityPeriod} -> signatureValidityPeriod) (\s@SigningProfile' {} a -> s {signatureValidityPeriod = a} :: SigningProfile)

-- | The parameters that are available for use by a code signing user.
signingProfile_signingParameters :: Lens.Lens' SigningProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signingProfile_signingParameters = Lens.lens (\SigningProfile' {signingParameters} -> signingParameters) (\s@SigningProfile' {} a -> s {signingParameters = a} :: SigningProfile) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags associated with the signing profile.
signingProfile_tags :: Lens.Lens' SigningProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signingProfile_tags = Lens.lens (\SigningProfile' {tags} -> tags) (\s@SigningProfile' {} a -> s {tags = a} :: SigningProfile) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SigningProfile where
  parseJSON =
    Core.withObject
      "SigningProfile"
      ( \x ->
          SigningProfile'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "platformDisplayName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "signingMaterial")
            Prelude.<*> (x Core..:? "profileVersion")
            Prelude.<*> (x Core..:? "profileName")
            Prelude.<*> (x Core..:? "profileVersionArn")
            Prelude.<*> (x Core..:? "platformId")
            Prelude.<*> (x Core..:? "signatureValidityPeriod")
            Prelude.<*> ( x Core..:? "signingParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SigningProfile

instance Prelude.NFData SigningProfile
