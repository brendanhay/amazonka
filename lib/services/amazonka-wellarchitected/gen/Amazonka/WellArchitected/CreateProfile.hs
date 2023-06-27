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
-- Module      : Amazonka.WellArchitected.CreateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a profile.
module Amazonka.WellArchitected.CreateProfile
  ( -- * Creating a Request
    CreateProfile (..),
    newCreateProfile,

    -- * Request Lenses
    createProfile_tags,
    createProfile_profileName,
    createProfile_profileDescription,
    createProfile_profileQuestions,
    createProfile_clientRequestToken,

    -- * Destructuring the Response
    CreateProfileResponse (..),
    newCreateProfileResponse,

    -- * Response Lenses
    createProfileResponse_profileArn,
    createProfileResponse_profileVersion,
    createProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The tags assigned to the profile.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Name of the profile.
    profileName :: Prelude.Text,
    -- | The profile description.
    profileDescription :: Prelude.Text,
    -- | The profile questions.
    profileQuestions :: [ProfileQuestionUpdate],
    clientRequestToken :: Prelude.Text
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
-- 'tags', 'createProfile_tags' - The tags assigned to the profile.
--
-- 'profileName', 'createProfile_profileName' - Name of the profile.
--
-- 'profileDescription', 'createProfile_profileDescription' - The profile description.
--
-- 'profileQuestions', 'createProfile_profileQuestions' - The profile questions.
--
-- 'clientRequestToken', 'createProfile_clientRequestToken' - Undocumented member.
newCreateProfile ::
  -- | 'profileName'
  Prelude.Text ->
  -- | 'profileDescription'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateProfile
newCreateProfile
  pProfileName_
  pProfileDescription_
  pClientRequestToken_ =
    CreateProfile'
      { tags = Prelude.Nothing,
        profileName = pProfileName_,
        profileDescription = pProfileDescription_,
        profileQuestions = Prelude.mempty,
        clientRequestToken = pClientRequestToken_
      }

-- | The tags assigned to the profile.
createProfile_tags :: Lens.Lens' CreateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProfile_tags = Lens.lens (\CreateProfile' {tags} -> tags) (\s@CreateProfile' {} a -> s {tags = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | Name of the profile.
createProfile_profileName :: Lens.Lens' CreateProfile Prelude.Text
createProfile_profileName = Lens.lens (\CreateProfile' {profileName} -> profileName) (\s@CreateProfile' {} a -> s {profileName = a} :: CreateProfile)

-- | The profile description.
createProfile_profileDescription :: Lens.Lens' CreateProfile Prelude.Text
createProfile_profileDescription = Lens.lens (\CreateProfile' {profileDescription} -> profileDescription) (\s@CreateProfile' {} a -> s {profileDescription = a} :: CreateProfile)

-- | The profile questions.
createProfile_profileQuestions :: Lens.Lens' CreateProfile [ProfileQuestionUpdate]
createProfile_profileQuestions = Lens.lens (\CreateProfile' {profileQuestions} -> profileQuestions) (\s@CreateProfile' {} a -> s {profileQuestions = a} :: CreateProfile) Prelude.. Lens.coerced

-- | Undocumented member.
createProfile_clientRequestToken :: Lens.Lens' CreateProfile Prelude.Text
createProfile_clientRequestToken = Lens.lens (\CreateProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateProfile' {} a -> s {clientRequestToken = a} :: CreateProfile)

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
            Prelude.<$> (x Data..?> "ProfileArn")
            Prelude.<*> (x Data..?> "ProfileVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProfile where
  hashWithSalt _salt CreateProfile' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` profileDescription
      `Prelude.hashWithSalt` profileQuestions
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateProfile where
  rnf CreateProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf profileDescription
      `Prelude.seq` Prelude.rnf profileQuestions
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ProfileName" Data..= profileName),
            Prelude.Just
              ("ProfileDescription" Data..= profileDescription),
            Prelude.Just
              ("ProfileQuestions" Data..= profileQuestions),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateProfile where
  toPath = Prelude.const "/profiles"

instance Data.ToQuery CreateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The profile ARN.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | Version of the profile.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'profileArn', 'createProfileResponse_profileArn' - The profile ARN.
--
-- 'profileVersion', 'createProfileResponse_profileVersion' - Version of the profile.
--
-- 'httpStatus', 'createProfileResponse_httpStatus' - The response's http status code.
newCreateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProfileResponse
newCreateProfileResponse pHttpStatus_ =
  CreateProfileResponse'
    { profileArn =
        Prelude.Nothing,
      profileVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The profile ARN.
createProfileResponse_profileArn :: Lens.Lens' CreateProfileResponse (Prelude.Maybe Prelude.Text)
createProfileResponse_profileArn = Lens.lens (\CreateProfileResponse' {profileArn} -> profileArn) (\s@CreateProfileResponse' {} a -> s {profileArn = a} :: CreateProfileResponse)

-- | Version of the profile.
createProfileResponse_profileVersion :: Lens.Lens' CreateProfileResponse (Prelude.Maybe Prelude.Text)
createProfileResponse_profileVersion = Lens.lens (\CreateProfileResponse' {profileVersion} -> profileVersion) (\s@CreateProfileResponse' {} a -> s {profileVersion = a} :: CreateProfileResponse)

-- | The response's http status code.
createProfileResponse_httpStatus :: Lens.Lens' CreateProfileResponse Prelude.Int
createProfileResponse_httpStatus = Lens.lens (\CreateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateProfileResponse' {} a -> s {httpStatus = a} :: CreateProfileResponse)

instance Prelude.NFData CreateProfileResponse where
  rnf CreateProfileResponse' {..} =
    Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf httpStatus
