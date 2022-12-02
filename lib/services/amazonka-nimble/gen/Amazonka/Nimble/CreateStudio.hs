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
-- Module      : Amazonka.Nimble.CreateStudio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new Studio.
--
-- When creating a Studio, two IAM roles must be provided: the admin role
-- and the user Role. These roles are assumed by your users when they log
-- in to the Nimble Studio portal.
--
-- The user role must have the AmazonNimbleStudio-StudioUser managed policy
-- attached for the portal to function properly.
--
-- The Admin Role must have the AmazonNimbleStudio-StudioAdmin managed
-- policy attached for the portal to function properly.
--
-- You may optionally specify a KMS key in the
-- StudioEncryptionConfiguration.
--
-- In Nimble Studio, resource names, descriptions, initialization scripts,
-- and other data you provide are always encrypted at rest using an KMS
-- key. By default, this key is owned by Amazon Web Services and managed on
-- your behalf. You may provide your own KMS key when calling CreateStudio
-- to encrypt this data using a key you own and manage.
--
-- When providing an KMS key during studio creation, Nimble Studio creates
-- KMS grants in your account to provide your studio user and admin roles
-- access to these KMS keys.
--
-- If you delete this grant, the studio will no longer be accessible to
-- your portal users.
--
-- If you delete the studio KMS key, your studio will no longer be
-- accessible.
module Amazonka.Nimble.CreateStudio
  ( -- * Creating a Request
    CreateStudio (..),
    newCreateStudio,

    -- * Request Lenses
    createStudio_tags,
    createStudio_clientToken,
    createStudio_studioEncryptionConfiguration,
    createStudio_adminRoleArn,
    createStudio_displayName,
    createStudio_studioName,
    createStudio_userRoleArn,

    -- * Destructuring the Response
    CreateStudioResponse (..),
    newCreateStudioResponse,

    -- * Response Lenses
    createStudioResponse_studio,
    createStudioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the AWS
    -- SDK automatically generates a client token and uses it for the request
    -- to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio encryption configuration.
    studioEncryptionConfiguration :: Prelude.Maybe StudioEncryptionConfiguration,
    -- | The IAM role that Studio Admins will assume when logging in to the
    -- Nimble Studio portal.
    adminRoleArn :: Prelude.Text,
    -- | A friendly name for the studio.
    displayName :: Data.Sensitive Prelude.Text,
    -- | The studio name that is used in the URL of the Nimble Studio portal when
    -- accessed by Nimble Studio users.
    studioName :: Prelude.Text,
    -- | The IAM role that Studio Users will assume when logging in to the Nimble
    -- Studio portal.
    userRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createStudio_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'clientToken', 'createStudio_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
--
-- 'studioEncryptionConfiguration', 'createStudio_studioEncryptionConfiguration' - The studio encryption configuration.
--
-- 'adminRoleArn', 'createStudio_adminRoleArn' - The IAM role that Studio Admins will assume when logging in to the
-- Nimble Studio portal.
--
-- 'displayName', 'createStudio_displayName' - A friendly name for the studio.
--
-- 'studioName', 'createStudio_studioName' - The studio name that is used in the URL of the Nimble Studio portal when
-- accessed by Nimble Studio users.
--
-- 'userRoleArn', 'createStudio_userRoleArn' - The IAM role that Studio Users will assume when logging in to the Nimble
-- Studio portal.
newCreateStudio ::
  -- | 'adminRoleArn'
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  -- | 'studioName'
  Prelude.Text ->
  -- | 'userRoleArn'
  Prelude.Text ->
  CreateStudio
newCreateStudio
  pAdminRoleArn_
  pDisplayName_
  pStudioName_
  pUserRoleArn_ =
    CreateStudio'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        studioEncryptionConfiguration = Prelude.Nothing,
        adminRoleArn = pAdminRoleArn_,
        displayName = Data._Sensitive Lens.# pDisplayName_,
        studioName = pStudioName_,
        userRoleArn = pUserRoleArn_
      }

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createStudio_tags :: Lens.Lens' CreateStudio (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStudio_tags = Lens.lens (\CreateStudio' {tags} -> tags) (\s@CreateStudio' {} a -> s {tags = a} :: CreateStudio) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the AWS
-- SDK automatically generates a client token and uses it for the request
-- to ensure idempotency.
createStudio_clientToken :: Lens.Lens' CreateStudio (Prelude.Maybe Prelude.Text)
createStudio_clientToken = Lens.lens (\CreateStudio' {clientToken} -> clientToken) (\s@CreateStudio' {} a -> s {clientToken = a} :: CreateStudio)

-- | The studio encryption configuration.
createStudio_studioEncryptionConfiguration :: Lens.Lens' CreateStudio (Prelude.Maybe StudioEncryptionConfiguration)
createStudio_studioEncryptionConfiguration = Lens.lens (\CreateStudio' {studioEncryptionConfiguration} -> studioEncryptionConfiguration) (\s@CreateStudio' {} a -> s {studioEncryptionConfiguration = a} :: CreateStudio)

-- | The IAM role that Studio Admins will assume when logging in to the
-- Nimble Studio portal.
createStudio_adminRoleArn :: Lens.Lens' CreateStudio Prelude.Text
createStudio_adminRoleArn = Lens.lens (\CreateStudio' {adminRoleArn} -> adminRoleArn) (\s@CreateStudio' {} a -> s {adminRoleArn = a} :: CreateStudio)

-- | A friendly name for the studio.
createStudio_displayName :: Lens.Lens' CreateStudio Prelude.Text
createStudio_displayName = Lens.lens (\CreateStudio' {displayName} -> displayName) (\s@CreateStudio' {} a -> s {displayName = a} :: CreateStudio) Prelude.. Data._Sensitive

-- | The studio name that is used in the URL of the Nimble Studio portal when
-- accessed by Nimble Studio users.
createStudio_studioName :: Lens.Lens' CreateStudio Prelude.Text
createStudio_studioName = Lens.lens (\CreateStudio' {studioName} -> studioName) (\s@CreateStudio' {} a -> s {studioName = a} :: CreateStudio)

-- | The IAM role that Studio Users will assume when logging in to the Nimble
-- Studio portal.
createStudio_userRoleArn :: Lens.Lens' CreateStudio Prelude.Text
createStudio_userRoleArn = Lens.lens (\CreateStudio' {userRoleArn} -> userRoleArn) (\s@CreateStudio' {} a -> s {userRoleArn = a} :: CreateStudio)

instance Core.AWSRequest CreateStudio where
  type AWSResponse CreateStudio = CreateStudioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            Prelude.<$> (x Data..?> "studio")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStudio where
  hashWithSalt _salt CreateStudio' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` studioEncryptionConfiguration
      `Prelude.hashWithSalt` adminRoleArn
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` studioName
      `Prelude.hashWithSalt` userRoleArn

instance Prelude.NFData CreateStudio where
  rnf CreateStudio' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf studioEncryptionConfiguration
      `Prelude.seq` Prelude.rnf adminRoleArn
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf studioName
      `Prelude.seq` Prelude.rnf userRoleArn

instance Data.ToHeaders CreateStudio where
  toHeaders CreateStudio' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateStudio where
  toJSON CreateStudio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("studioEncryptionConfiguration" Data..=)
              Prelude.<$> studioEncryptionConfiguration,
            Prelude.Just ("adminRoleArn" Data..= adminRoleArn),
            Prelude.Just ("displayName" Data..= displayName),
            Prelude.Just ("studioName" Data..= studioName),
            Prelude.Just ("userRoleArn" Data..= userRoleArn)
          ]
      )

instance Data.ToPath CreateStudio where
  toPath = Prelude.const "/2020-08-01/studios"

instance Data.ToQuery CreateStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { -- | Information about a studio.
    studio :: Prelude.Maybe Studio,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studio', 'createStudioResponse_studio' - Information about a studio.
--
-- 'httpStatus', 'createStudioResponse_httpStatus' - The response's http status code.
newCreateStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStudioResponse
newCreateStudioResponse pHttpStatus_ =
  CreateStudioResponse'
    { studio = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a studio.
createStudioResponse_studio :: Lens.Lens' CreateStudioResponse (Prelude.Maybe Studio)
createStudioResponse_studio = Lens.lens (\CreateStudioResponse' {studio} -> studio) (\s@CreateStudioResponse' {} a -> s {studio = a} :: CreateStudioResponse)

-- | The response's http status code.
createStudioResponse_httpStatus :: Lens.Lens' CreateStudioResponse Prelude.Int
createStudioResponse_httpStatus = Lens.lens (\CreateStudioResponse' {httpStatus} -> httpStatus) (\s@CreateStudioResponse' {} a -> s {httpStatus = a} :: CreateStudioResponse)

instance Prelude.NFData CreateStudioResponse where
  rnf CreateStudioResponse' {..} =
    Prelude.rnf studio
      `Prelude.seq` Prelude.rnf httpStatus
