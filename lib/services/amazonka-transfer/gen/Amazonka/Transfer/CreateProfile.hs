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
-- Module      : Amazonka.Transfer.CreateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the local or partner profile to use for AS2 transfers.
module Amazonka.Transfer.CreateProfile
  ( -- * Creating a Request
    CreateProfile (..),
    newCreateProfile,

    -- * Request Lenses
    createProfile_tags,
    createProfile_certificateIds,
    createProfile_as2Id,
    createProfile_profileType,

    -- * Destructuring the Response
    CreateProfileResponse (..),
    newCreateProfileResponse,

    -- * Response Lenses
    createProfileResponse_httpStatus,
    createProfileResponse_profileId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | Key-value pairs that can be used to group and search for AS2 profiles.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | An array of identifiers for the imported certificates. You use this
    -- identifier for working with profiles and partner profiles.
    certificateIds :: Prelude.Maybe [Prelude.Text],
    -- | The @As2Id@ is the /AS2-name/, as defined in the
    -- <https://datatracker.ietf.org/doc/html/rfc4130 RFC 4130>. For inbound
    -- transfers, this is the @AS2-From@ header for the AS2 messages sent from
    -- the partner. For outbound connectors, this is the @AS2-To@ header for
    -- the AS2 messages sent to the partner using the @StartFileTransfer@ API
    -- operation. This ID cannot include spaces.
    as2Id :: Prelude.Text,
    -- | Determines the type of profile to create:
    --
    -- -   Specify @LOCAL@ to create a local profile. A local profile
    --     represents the AS2-enabled Transfer Family server organization or
    --     party.
    --
    -- -   Specify @PARTNER@ to create a partner profile. A partner profile
    --     represents a remote organization, external to Transfer Family.
    profileType :: ProfileType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createProfile_tags' - Key-value pairs that can be used to group and search for AS2 profiles.
--
-- 'certificateIds', 'createProfile_certificateIds' - An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
--
-- 'as2Id', 'createProfile_as2Id' - The @As2Id@ is the /AS2-name/, as defined in the
-- <https://datatracker.ietf.org/doc/html/rfc4130 RFC 4130>. For inbound
-- transfers, this is the @AS2-From@ header for the AS2 messages sent from
-- the partner. For outbound connectors, this is the @AS2-To@ header for
-- the AS2 messages sent to the partner using the @StartFileTransfer@ API
-- operation. This ID cannot include spaces.
--
-- 'profileType', 'createProfile_profileType' - Determines the type of profile to create:
--
-- -   Specify @LOCAL@ to create a local profile. A local profile
--     represents the AS2-enabled Transfer Family server organization or
--     party.
--
-- -   Specify @PARTNER@ to create a partner profile. A partner profile
--     represents a remote organization, external to Transfer Family.
newCreateProfile ::
  -- | 'as2Id'
  Prelude.Text ->
  -- | 'profileType'
  ProfileType ->
  CreateProfile
newCreateProfile pAs2Id_ pProfileType_ =
  CreateProfile'
    { tags = Prelude.Nothing,
      certificateIds = Prelude.Nothing,
      as2Id = pAs2Id_,
      profileType = pProfileType_
    }

-- | Key-value pairs that can be used to group and search for AS2 profiles.
createProfile_tags :: Lens.Lens' CreateProfile (Prelude.Maybe (Prelude.NonEmpty Tag))
createProfile_tags = Lens.lens (\CreateProfile' {tags} -> tags) (\s@CreateProfile' {} a -> s {tags = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
createProfile_certificateIds :: Lens.Lens' CreateProfile (Prelude.Maybe [Prelude.Text])
createProfile_certificateIds = Lens.lens (\CreateProfile' {certificateIds} -> certificateIds) (\s@CreateProfile' {} a -> s {certificateIds = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The @As2Id@ is the /AS2-name/, as defined in the
-- <https://datatracker.ietf.org/doc/html/rfc4130 RFC 4130>. For inbound
-- transfers, this is the @AS2-From@ header for the AS2 messages sent from
-- the partner. For outbound connectors, this is the @AS2-To@ header for
-- the AS2 messages sent to the partner using the @StartFileTransfer@ API
-- operation. This ID cannot include spaces.
createProfile_as2Id :: Lens.Lens' CreateProfile Prelude.Text
createProfile_as2Id = Lens.lens (\CreateProfile' {as2Id} -> as2Id) (\s@CreateProfile' {} a -> s {as2Id = a} :: CreateProfile)

-- | Determines the type of profile to create:
--
-- -   Specify @LOCAL@ to create a local profile. A local profile
--     represents the AS2-enabled Transfer Family server organization or
--     party.
--
-- -   Specify @PARTNER@ to create a partner profile. A partner profile
--     represents a remote organization, external to Transfer Family.
createProfile_profileType :: Lens.Lens' CreateProfile ProfileType
createProfile_profileType = Lens.lens (\CreateProfile' {profileType} -> profileType) (\s@CreateProfile' {} a -> s {profileType = a} :: CreateProfile)

instance Core.AWSRequest CreateProfile where
  type
    AWSResponse CreateProfile =
      CreateProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ProfileId")
      )

instance Prelude.Hashable CreateProfile where
  hashWithSalt _salt CreateProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` certificateIds
      `Prelude.hashWithSalt` as2Id
      `Prelude.hashWithSalt` profileType

instance Prelude.NFData CreateProfile where
  rnf CreateProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf certificateIds
      `Prelude.seq` Prelude.rnf as2Id
      `Prelude.seq` Prelude.rnf profileType

instance Core.ToHeaders CreateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.CreateProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("CertificateIds" Core..=)
              Prelude.<$> certificateIds,
            Prelude.Just ("As2Id" Core..= as2Id),
            Prelude.Just ("ProfileType" Core..= profileType)
          ]
      )

instance Core.ToPath CreateProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier for the AS2 profile, returned after the API call
    -- succeeds.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProfileResponse_httpStatus' - The response's http status code.
--
-- 'profileId', 'createProfileResponse_profileId' - The unique identifier for the AS2 profile, returned after the API call
-- succeeds.
newCreateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profileId'
  Prelude.Text ->
  CreateProfileResponse
newCreateProfileResponse pHttpStatus_ pProfileId_ =
  CreateProfileResponse'
    { httpStatus = pHttpStatus_,
      profileId = pProfileId_
    }

-- | The response's http status code.
createProfileResponse_httpStatus :: Lens.Lens' CreateProfileResponse Prelude.Int
createProfileResponse_httpStatus = Lens.lens (\CreateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateProfileResponse' {} a -> s {httpStatus = a} :: CreateProfileResponse)

-- | The unique identifier for the AS2 profile, returned after the API call
-- succeeds.
createProfileResponse_profileId :: Lens.Lens' CreateProfileResponse Prelude.Text
createProfileResponse_profileId = Lens.lens (\CreateProfileResponse' {profileId} -> profileId) (\s@CreateProfileResponse' {} a -> s {profileId = a} :: CreateProfileResponse)

instance Prelude.NFData CreateProfileResponse where
  rnf CreateProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profileId
