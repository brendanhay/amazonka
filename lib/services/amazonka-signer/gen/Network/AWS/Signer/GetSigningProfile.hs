{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Signer.GetSigningProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on a specific signing profile.
module Network.AWS.Signer.GetSigningProfile
  ( -- * Creating a Request
    GetSigningProfile (..),
    newGetSigningProfile,

    -- * Request Lenses
    getSigningProfile_profileOwner,
    getSigningProfile_profileName,

    -- * Destructuring the Response
    GetSigningProfileResponse (..),
    newGetSigningProfileResponse,

    -- * Response Lenses
    getSigningProfileResponse_status,
    getSigningProfileResponse_overrides,
    getSigningProfileResponse_platformDisplayName,
    getSigningProfileResponse_arn,
    getSigningProfileResponse_signingMaterial,
    getSigningProfileResponse_profileVersion,
    getSigningProfileResponse_profileName,
    getSigningProfileResponse_profileVersionArn,
    getSigningProfileResponse_platformId,
    getSigningProfileResponse_revocationRecord,
    getSigningProfileResponse_statusReason,
    getSigningProfileResponse_signatureValidityPeriod,
    getSigningProfileResponse_signingParameters,
    getSigningProfileResponse_tags,
    getSigningProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Signer.Types

-- | /See:/ 'newGetSigningProfile' smart constructor.
data GetSigningProfile = GetSigningProfile'
  { -- | The AWS account ID of the profile owner.
    profileOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the target signing profile.
    profileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSigningProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileOwner', 'getSigningProfile_profileOwner' - The AWS account ID of the profile owner.
--
-- 'profileName', 'getSigningProfile_profileName' - The name of the target signing profile.
newGetSigningProfile ::
  -- | 'profileName'
  Prelude.Text ->
  GetSigningProfile
newGetSigningProfile pProfileName_ =
  GetSigningProfile'
    { profileOwner = Prelude.Nothing,
      profileName = pProfileName_
    }

-- | The AWS account ID of the profile owner.
getSigningProfile_profileOwner :: Lens.Lens' GetSigningProfile (Prelude.Maybe Prelude.Text)
getSigningProfile_profileOwner = Lens.lens (\GetSigningProfile' {profileOwner} -> profileOwner) (\s@GetSigningProfile' {} a -> s {profileOwner = a} :: GetSigningProfile)

-- | The name of the target signing profile.
getSigningProfile_profileName :: Lens.Lens' GetSigningProfile Prelude.Text
getSigningProfile_profileName = Lens.lens (\GetSigningProfile' {profileName} -> profileName) (\s@GetSigningProfile' {} a -> s {profileName = a} :: GetSigningProfile)

instance Core.AWSRequest GetSigningProfile where
  type
    AWSResponse GetSigningProfile =
      GetSigningProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSigningProfileResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "overrides")
            Prelude.<*> (x Core..?> "platformDisplayName")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "signingMaterial")
            Prelude.<*> (x Core..?> "profileVersion")
            Prelude.<*> (x Core..?> "profileName")
            Prelude.<*> (x Core..?> "profileVersionArn")
            Prelude.<*> (x Core..?> "platformId")
            Prelude.<*> (x Core..?> "revocationRecord")
            Prelude.<*> (x Core..?> "statusReason")
            Prelude.<*> (x Core..?> "signatureValidityPeriod")
            Prelude.<*> ( x Core..?> "signingParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSigningProfile

instance Prelude.NFData GetSigningProfile

instance Core.ToHeaders GetSigningProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSigningProfile where
  toPath GetSigningProfile' {..} =
    Prelude.mconcat
      ["/signing-profiles/", Core.toBS profileName]

instance Core.ToQuery GetSigningProfile where
  toQuery GetSigningProfile' {..} =
    Prelude.mconcat
      ["profileOwner" Core.=: profileOwner]

-- | /See:/ 'newGetSigningProfileResponse' smart constructor.
data GetSigningProfileResponse = GetSigningProfileResponse'
  { -- | The status of the target signing profile.
    status :: Prelude.Maybe SigningProfileStatus,
    -- | A list of overrides applied by the target signing profile for signing
    -- operations.
    overrides :: Prelude.Maybe SigningPlatformOverrides,
    -- | A human-readable name for the signing platform associated with the
    -- signing profile.
    platformDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the signing profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the certificate that the target profile uses for signing
    -- operations.
    signingMaterial :: Prelude.Maybe SigningMaterial,
    -- | The current version of the signing profile.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the target signing profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The signing profile ARN, including the profile version.
    profileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the platform that is used by the target signing profile.
    platformId :: Prelude.Maybe Prelude.Text,
    revocationRecord :: Prelude.Maybe SigningProfileRevocationRecord,
    -- | Reason for the status of the target signing profile.
    statusReason :: Prelude.Maybe Prelude.Text,
    signatureValidityPeriod :: Prelude.Maybe SignatureValidityPeriod,
    -- | A map of key-value pairs for signing operations that is attached to the
    -- target signing profile.
    signingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of tags associated with the signing profile.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSigningProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getSigningProfileResponse_status' - The status of the target signing profile.
--
-- 'overrides', 'getSigningProfileResponse_overrides' - A list of overrides applied by the target signing profile for signing
-- operations.
--
-- 'platformDisplayName', 'getSigningProfileResponse_platformDisplayName' - A human-readable name for the signing platform associated with the
-- signing profile.
--
-- 'arn', 'getSigningProfileResponse_arn' - The Amazon Resource Name (ARN) for the signing profile.
--
-- 'signingMaterial', 'getSigningProfileResponse_signingMaterial' - The ARN of the certificate that the target profile uses for signing
-- operations.
--
-- 'profileVersion', 'getSigningProfileResponse_profileVersion' - The current version of the signing profile.
--
-- 'profileName', 'getSigningProfileResponse_profileName' - The name of the target signing profile.
--
-- 'profileVersionArn', 'getSigningProfileResponse_profileVersionArn' - The signing profile ARN, including the profile version.
--
-- 'platformId', 'getSigningProfileResponse_platformId' - The ID of the platform that is used by the target signing profile.
--
-- 'revocationRecord', 'getSigningProfileResponse_revocationRecord' - Undocumented member.
--
-- 'statusReason', 'getSigningProfileResponse_statusReason' - Reason for the status of the target signing profile.
--
-- 'signatureValidityPeriod', 'getSigningProfileResponse_signatureValidityPeriod' - Undocumented member.
--
-- 'signingParameters', 'getSigningProfileResponse_signingParameters' - A map of key-value pairs for signing operations that is attached to the
-- target signing profile.
--
-- 'tags', 'getSigningProfileResponse_tags' - A list of tags associated with the signing profile.
--
-- 'httpStatus', 'getSigningProfileResponse_httpStatus' - The response's http status code.
newGetSigningProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSigningProfileResponse
newGetSigningProfileResponse pHttpStatus_ =
  GetSigningProfileResponse'
    { status =
        Prelude.Nothing,
      overrides = Prelude.Nothing,
      platformDisplayName = Prelude.Nothing,
      arn = Prelude.Nothing,
      signingMaterial = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      profileName = Prelude.Nothing,
      profileVersionArn = Prelude.Nothing,
      platformId = Prelude.Nothing,
      revocationRecord = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      signatureValidityPeriod = Prelude.Nothing,
      signingParameters = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the target signing profile.
getSigningProfileResponse_status :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe SigningProfileStatus)
getSigningProfileResponse_status = Lens.lens (\GetSigningProfileResponse' {status} -> status) (\s@GetSigningProfileResponse' {} a -> s {status = a} :: GetSigningProfileResponse)

-- | A list of overrides applied by the target signing profile for signing
-- operations.
getSigningProfileResponse_overrides :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe SigningPlatformOverrides)
getSigningProfileResponse_overrides = Lens.lens (\GetSigningProfileResponse' {overrides} -> overrides) (\s@GetSigningProfileResponse' {} a -> s {overrides = a} :: GetSigningProfileResponse)

-- | A human-readable name for the signing platform associated with the
-- signing profile.
getSigningProfileResponse_platformDisplayName :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_platformDisplayName = Lens.lens (\GetSigningProfileResponse' {platformDisplayName} -> platformDisplayName) (\s@GetSigningProfileResponse' {} a -> s {platformDisplayName = a} :: GetSigningProfileResponse)

-- | The Amazon Resource Name (ARN) for the signing profile.
getSigningProfileResponse_arn :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_arn = Lens.lens (\GetSigningProfileResponse' {arn} -> arn) (\s@GetSigningProfileResponse' {} a -> s {arn = a} :: GetSigningProfileResponse)

-- | The ARN of the certificate that the target profile uses for signing
-- operations.
getSigningProfileResponse_signingMaterial :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe SigningMaterial)
getSigningProfileResponse_signingMaterial = Lens.lens (\GetSigningProfileResponse' {signingMaterial} -> signingMaterial) (\s@GetSigningProfileResponse' {} a -> s {signingMaterial = a} :: GetSigningProfileResponse)

-- | The current version of the signing profile.
getSigningProfileResponse_profileVersion :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_profileVersion = Lens.lens (\GetSigningProfileResponse' {profileVersion} -> profileVersion) (\s@GetSigningProfileResponse' {} a -> s {profileVersion = a} :: GetSigningProfileResponse)

-- | The name of the target signing profile.
getSigningProfileResponse_profileName :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_profileName = Lens.lens (\GetSigningProfileResponse' {profileName} -> profileName) (\s@GetSigningProfileResponse' {} a -> s {profileName = a} :: GetSigningProfileResponse)

-- | The signing profile ARN, including the profile version.
getSigningProfileResponse_profileVersionArn :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_profileVersionArn = Lens.lens (\GetSigningProfileResponse' {profileVersionArn} -> profileVersionArn) (\s@GetSigningProfileResponse' {} a -> s {profileVersionArn = a} :: GetSigningProfileResponse)

-- | The ID of the platform that is used by the target signing profile.
getSigningProfileResponse_platformId :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_platformId = Lens.lens (\GetSigningProfileResponse' {platformId} -> platformId) (\s@GetSigningProfileResponse' {} a -> s {platformId = a} :: GetSigningProfileResponse)

-- | Undocumented member.
getSigningProfileResponse_revocationRecord :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe SigningProfileRevocationRecord)
getSigningProfileResponse_revocationRecord = Lens.lens (\GetSigningProfileResponse' {revocationRecord} -> revocationRecord) (\s@GetSigningProfileResponse' {} a -> s {revocationRecord = a} :: GetSigningProfileResponse)

-- | Reason for the status of the target signing profile.
getSigningProfileResponse_statusReason :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe Prelude.Text)
getSigningProfileResponse_statusReason = Lens.lens (\GetSigningProfileResponse' {statusReason} -> statusReason) (\s@GetSigningProfileResponse' {} a -> s {statusReason = a} :: GetSigningProfileResponse)

-- | Undocumented member.
getSigningProfileResponse_signatureValidityPeriod :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe SignatureValidityPeriod)
getSigningProfileResponse_signatureValidityPeriod = Lens.lens (\GetSigningProfileResponse' {signatureValidityPeriod} -> signatureValidityPeriod) (\s@GetSigningProfileResponse' {} a -> s {signatureValidityPeriod = a} :: GetSigningProfileResponse)

-- | A map of key-value pairs for signing operations that is attached to the
-- target signing profile.
getSigningProfileResponse_signingParameters :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSigningProfileResponse_signingParameters = Lens.lens (\GetSigningProfileResponse' {signingParameters} -> signingParameters) (\s@GetSigningProfileResponse' {} a -> s {signingParameters = a} :: GetSigningProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags associated with the signing profile.
getSigningProfileResponse_tags :: Lens.Lens' GetSigningProfileResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSigningProfileResponse_tags = Lens.lens (\GetSigningProfileResponse' {tags} -> tags) (\s@GetSigningProfileResponse' {} a -> s {tags = a} :: GetSigningProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSigningProfileResponse_httpStatus :: Lens.Lens' GetSigningProfileResponse Prelude.Int
getSigningProfileResponse_httpStatus = Lens.lens (\GetSigningProfileResponse' {httpStatus} -> httpStatus) (\s@GetSigningProfileResponse' {} a -> s {httpStatus = a} :: GetSigningProfileResponse)

instance Prelude.NFData GetSigningProfileResponse
