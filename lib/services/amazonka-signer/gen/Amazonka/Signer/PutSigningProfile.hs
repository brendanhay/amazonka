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
-- Module      : Amazonka.Signer.PutSigningProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signing profile. A signing profile is a code signing template
-- that can be used to carry out a pre-defined signing job. For more
-- information, see
-- <http://docs.aws.amazon.com/signer/latest/developerguide/gs-profile.html>
module Amazonka.Signer.PutSigningProfile
  ( -- * Creating a Request
    PutSigningProfile (..),
    newPutSigningProfile,

    -- * Request Lenses
    putSigningProfile_tags,
    putSigningProfile_signatureValidityPeriod,
    putSigningProfile_signingParameters,
    putSigningProfile_signingMaterial,
    putSigningProfile_overrides,
    putSigningProfile_profileName,
    putSigningProfile_platformId,

    -- * Destructuring the Response
    PutSigningProfileResponse (..),
    newPutSigningProfileResponse,

    -- * Response Lenses
    putSigningProfileResponse_profileVersionArn,
    putSigningProfileResponse_arn,
    putSigningProfileResponse_profileVersion,
    putSigningProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newPutSigningProfile' smart constructor.
data PutSigningProfile = PutSigningProfile'
  { -- | Tags to be associated with the signing profile that is being created.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The default validity period override for any signature generated using
    -- this signing profile. If unspecified, the default is 135 months.
    signatureValidityPeriod :: Prelude.Maybe SignatureValidityPeriod,
    -- | Map of key-value pairs for signing. These can include any information
    -- that you want to use during signing.
    signingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The AWS Certificate Manager certificate that will be used to sign code
    -- with the new signing profile.
    signingMaterial :: Prelude.Maybe SigningMaterial,
    -- | A subfield of @platform@. This specifies any different configuration
    -- options that you want to apply to the chosen platform (such as a
    -- different @hash-algorithm@ or @signing-algorithm@).
    overrides :: Prelude.Maybe SigningPlatformOverrides,
    -- | The name of the signing profile to be created.
    profileName :: Prelude.Text,
    -- | The ID of the signing platform to be created.
    platformId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSigningProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putSigningProfile_tags' - Tags to be associated with the signing profile that is being created.
--
-- 'signatureValidityPeriod', 'putSigningProfile_signatureValidityPeriod' - The default validity period override for any signature generated using
-- this signing profile. If unspecified, the default is 135 months.
--
-- 'signingParameters', 'putSigningProfile_signingParameters' - Map of key-value pairs for signing. These can include any information
-- that you want to use during signing.
--
-- 'signingMaterial', 'putSigningProfile_signingMaterial' - The AWS Certificate Manager certificate that will be used to sign code
-- with the new signing profile.
--
-- 'overrides', 'putSigningProfile_overrides' - A subfield of @platform@. This specifies any different configuration
-- options that you want to apply to the chosen platform (such as a
-- different @hash-algorithm@ or @signing-algorithm@).
--
-- 'profileName', 'putSigningProfile_profileName' - The name of the signing profile to be created.
--
-- 'platformId', 'putSigningProfile_platformId' - The ID of the signing platform to be created.
newPutSigningProfile ::
  -- | 'profileName'
  Prelude.Text ->
  -- | 'platformId'
  Prelude.Text ->
  PutSigningProfile
newPutSigningProfile pProfileName_ pPlatformId_ =
  PutSigningProfile'
    { tags = Prelude.Nothing,
      signatureValidityPeriod = Prelude.Nothing,
      signingParameters = Prelude.Nothing,
      signingMaterial = Prelude.Nothing,
      overrides = Prelude.Nothing,
      profileName = pProfileName_,
      platformId = pPlatformId_
    }

-- | Tags to be associated with the signing profile that is being created.
putSigningProfile_tags :: Lens.Lens' PutSigningProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putSigningProfile_tags = Lens.lens (\PutSigningProfile' {tags} -> tags) (\s@PutSigningProfile' {} a -> s {tags = a} :: PutSigningProfile) Prelude.. Lens.mapping Lens.coerced

-- | The default validity period override for any signature generated using
-- this signing profile. If unspecified, the default is 135 months.
putSigningProfile_signatureValidityPeriod :: Lens.Lens' PutSigningProfile (Prelude.Maybe SignatureValidityPeriod)
putSigningProfile_signatureValidityPeriod = Lens.lens (\PutSigningProfile' {signatureValidityPeriod} -> signatureValidityPeriod) (\s@PutSigningProfile' {} a -> s {signatureValidityPeriod = a} :: PutSigningProfile)

-- | Map of key-value pairs for signing. These can include any information
-- that you want to use during signing.
putSigningProfile_signingParameters :: Lens.Lens' PutSigningProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putSigningProfile_signingParameters = Lens.lens (\PutSigningProfile' {signingParameters} -> signingParameters) (\s@PutSigningProfile' {} a -> s {signingParameters = a} :: PutSigningProfile) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Certificate Manager certificate that will be used to sign code
-- with the new signing profile.
putSigningProfile_signingMaterial :: Lens.Lens' PutSigningProfile (Prelude.Maybe SigningMaterial)
putSigningProfile_signingMaterial = Lens.lens (\PutSigningProfile' {signingMaterial} -> signingMaterial) (\s@PutSigningProfile' {} a -> s {signingMaterial = a} :: PutSigningProfile)

-- | A subfield of @platform@. This specifies any different configuration
-- options that you want to apply to the chosen platform (such as a
-- different @hash-algorithm@ or @signing-algorithm@).
putSigningProfile_overrides :: Lens.Lens' PutSigningProfile (Prelude.Maybe SigningPlatformOverrides)
putSigningProfile_overrides = Lens.lens (\PutSigningProfile' {overrides} -> overrides) (\s@PutSigningProfile' {} a -> s {overrides = a} :: PutSigningProfile)

-- | The name of the signing profile to be created.
putSigningProfile_profileName :: Lens.Lens' PutSigningProfile Prelude.Text
putSigningProfile_profileName = Lens.lens (\PutSigningProfile' {profileName} -> profileName) (\s@PutSigningProfile' {} a -> s {profileName = a} :: PutSigningProfile)

-- | The ID of the signing platform to be created.
putSigningProfile_platformId :: Lens.Lens' PutSigningProfile Prelude.Text
putSigningProfile_platformId = Lens.lens (\PutSigningProfile' {platformId} -> platformId) (\s@PutSigningProfile' {} a -> s {platformId = a} :: PutSigningProfile)

instance Core.AWSRequest PutSigningProfile where
  type
    AWSResponse PutSigningProfile =
      PutSigningProfileResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSigningProfileResponse'
            Prelude.<$> (x Data..?> "profileVersionArn")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "profileVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSigningProfile where
  hashWithSalt _salt PutSigningProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` signatureValidityPeriod
      `Prelude.hashWithSalt` signingParameters
      `Prelude.hashWithSalt` signingMaterial
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` platformId

instance Prelude.NFData PutSigningProfile where
  rnf PutSigningProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf signatureValidityPeriod
      `Prelude.seq` Prelude.rnf signingParameters
      `Prelude.seq` Prelude.rnf signingMaterial
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf platformId

instance Data.ToHeaders PutSigningProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutSigningProfile where
  toJSON PutSigningProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("signatureValidityPeriod" Data..=)
              Prelude.<$> signatureValidityPeriod,
            ("signingParameters" Data..=)
              Prelude.<$> signingParameters,
            ("signingMaterial" Data..=)
              Prelude.<$> signingMaterial,
            ("overrides" Data..=) Prelude.<$> overrides,
            Prelude.Just ("platformId" Data..= platformId)
          ]
      )

instance Data.ToPath PutSigningProfile where
  toPath PutSigningProfile' {..} =
    Prelude.mconcat
      ["/signing-profiles/", Data.toBS profileName]

instance Data.ToQuery PutSigningProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSigningProfileResponse' smart constructor.
data PutSigningProfileResponse = PutSigningProfileResponse'
  { -- | The signing profile ARN, including the profile version.
    profileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the signing profile created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version of the signing profile being created.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSigningProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileVersionArn', 'putSigningProfileResponse_profileVersionArn' - The signing profile ARN, including the profile version.
--
-- 'arn', 'putSigningProfileResponse_arn' - The Amazon Resource Name (ARN) of the signing profile created.
--
-- 'profileVersion', 'putSigningProfileResponse_profileVersion' - The version of the signing profile being created.
--
-- 'httpStatus', 'putSigningProfileResponse_httpStatus' - The response's http status code.
newPutSigningProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSigningProfileResponse
newPutSigningProfileResponse pHttpStatus_ =
  PutSigningProfileResponse'
    { profileVersionArn =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signing profile ARN, including the profile version.
putSigningProfileResponse_profileVersionArn :: Lens.Lens' PutSigningProfileResponse (Prelude.Maybe Prelude.Text)
putSigningProfileResponse_profileVersionArn = Lens.lens (\PutSigningProfileResponse' {profileVersionArn} -> profileVersionArn) (\s@PutSigningProfileResponse' {} a -> s {profileVersionArn = a} :: PutSigningProfileResponse)

-- | The Amazon Resource Name (ARN) of the signing profile created.
putSigningProfileResponse_arn :: Lens.Lens' PutSigningProfileResponse (Prelude.Maybe Prelude.Text)
putSigningProfileResponse_arn = Lens.lens (\PutSigningProfileResponse' {arn} -> arn) (\s@PutSigningProfileResponse' {} a -> s {arn = a} :: PutSigningProfileResponse)

-- | The version of the signing profile being created.
putSigningProfileResponse_profileVersion :: Lens.Lens' PutSigningProfileResponse (Prelude.Maybe Prelude.Text)
putSigningProfileResponse_profileVersion = Lens.lens (\PutSigningProfileResponse' {profileVersion} -> profileVersion) (\s@PutSigningProfileResponse' {} a -> s {profileVersion = a} :: PutSigningProfileResponse)

-- | The response's http status code.
putSigningProfileResponse_httpStatus :: Lens.Lens' PutSigningProfileResponse Prelude.Int
putSigningProfileResponse_httpStatus = Lens.lens (\PutSigningProfileResponse' {httpStatus} -> httpStatus) (\s@PutSigningProfileResponse' {} a -> s {httpStatus = a} :: PutSigningProfileResponse)

instance Prelude.NFData PutSigningProfileResponse where
  rnf PutSigningProfileResponse' {..} =
    Prelude.rnf profileVersionArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf httpStatus
