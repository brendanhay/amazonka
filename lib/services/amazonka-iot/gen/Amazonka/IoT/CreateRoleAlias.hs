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
-- Module      : Amazonka.IoT.CreateRoleAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a role alias.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateRoleAlias>
-- action.
module Amazonka.IoT.CreateRoleAlias
  ( -- * Creating a Request
    CreateRoleAlias (..),
    newCreateRoleAlias,

    -- * Request Lenses
    createRoleAlias_credentialDurationSeconds,
    createRoleAlias_tags,
    createRoleAlias_roleAlias,
    createRoleAlias_roleArn,

    -- * Destructuring the Response
    CreateRoleAliasResponse (..),
    newCreateRoleAliasResponse,

    -- * Response Lenses
    createRoleAliasResponse_roleAlias,
    createRoleAliasResponse_roleAliasArn,
    createRoleAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRoleAlias' smart constructor.
data CreateRoleAlias = CreateRoleAlias'
  { -- | How long (in seconds) the credentials will be valid. The default value
    -- is 3,600 seconds.
    --
    -- This value must be less than or equal to the maximum session duration of
    -- the IAM role that the role alias references.
    credentialDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Metadata which can be used to manage the role alias.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | The role alias that points to a role ARN. This allows you to change the
    -- role without having to update the device.
    roleAlias :: Prelude.Text,
    -- | The role ARN.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoleAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialDurationSeconds', 'createRoleAlias_credentialDurationSeconds' - How long (in seconds) the credentials will be valid. The default value
-- is 3,600 seconds.
--
-- This value must be less than or equal to the maximum session duration of
-- the IAM role that the role alias references.
--
-- 'tags', 'createRoleAlias_tags' - Metadata which can be used to manage the role alias.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'roleAlias', 'createRoleAlias_roleAlias' - The role alias that points to a role ARN. This allows you to change the
-- role without having to update the device.
--
-- 'roleArn', 'createRoleAlias_roleArn' - The role ARN.
newCreateRoleAlias ::
  -- | 'roleAlias'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateRoleAlias
newCreateRoleAlias pRoleAlias_ pRoleArn_ =
  CreateRoleAlias'
    { credentialDurationSeconds =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      roleAlias = pRoleAlias_,
      roleArn = pRoleArn_
    }

-- | How long (in seconds) the credentials will be valid. The default value
-- is 3,600 seconds.
--
-- This value must be less than or equal to the maximum session duration of
-- the IAM role that the role alias references.
createRoleAlias_credentialDurationSeconds :: Lens.Lens' CreateRoleAlias (Prelude.Maybe Prelude.Natural)
createRoleAlias_credentialDurationSeconds = Lens.lens (\CreateRoleAlias' {credentialDurationSeconds} -> credentialDurationSeconds) (\s@CreateRoleAlias' {} a -> s {credentialDurationSeconds = a} :: CreateRoleAlias)

-- | Metadata which can be used to manage the role alias.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createRoleAlias_tags :: Lens.Lens' CreateRoleAlias (Prelude.Maybe [Tag])
createRoleAlias_tags = Lens.lens (\CreateRoleAlias' {tags} -> tags) (\s@CreateRoleAlias' {} a -> s {tags = a} :: CreateRoleAlias) Prelude.. Lens.mapping Lens.coerced

-- | The role alias that points to a role ARN. This allows you to change the
-- role without having to update the device.
createRoleAlias_roleAlias :: Lens.Lens' CreateRoleAlias Prelude.Text
createRoleAlias_roleAlias = Lens.lens (\CreateRoleAlias' {roleAlias} -> roleAlias) (\s@CreateRoleAlias' {} a -> s {roleAlias = a} :: CreateRoleAlias)

-- | The role ARN.
createRoleAlias_roleArn :: Lens.Lens' CreateRoleAlias Prelude.Text
createRoleAlias_roleArn = Lens.lens (\CreateRoleAlias' {roleArn} -> roleArn) (\s@CreateRoleAlias' {} a -> s {roleArn = a} :: CreateRoleAlias)

instance Core.AWSRequest CreateRoleAlias where
  type
    AWSResponse CreateRoleAlias =
      CreateRoleAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoleAliasResponse'
            Prelude.<$> (x Data..?> "roleAlias")
            Prelude.<*> (x Data..?> "roleAliasArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoleAlias where
  hashWithSalt _salt CreateRoleAlias' {..} =
    _salt
      `Prelude.hashWithSalt` credentialDurationSeconds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roleAlias
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateRoleAlias where
  rnf CreateRoleAlias' {..} =
    Prelude.rnf credentialDurationSeconds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf roleAlias
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateRoleAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateRoleAlias where
  toJSON CreateRoleAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("credentialDurationSeconds" Data..=)
              Prelude.<$> credentialDurationSeconds,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateRoleAlias where
  toPath CreateRoleAlias' {..} =
    Prelude.mconcat
      ["/role-aliases/", Data.toBS roleAlias]

instance Data.ToQuery CreateRoleAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRoleAliasResponse' smart constructor.
data CreateRoleAliasResponse = CreateRoleAliasResponse'
  { -- | The role alias.
    roleAlias :: Prelude.Maybe Prelude.Text,
    -- | The role alias ARN.
    roleAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoleAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAlias', 'createRoleAliasResponse_roleAlias' - The role alias.
--
-- 'roleAliasArn', 'createRoleAliasResponse_roleAliasArn' - The role alias ARN.
--
-- 'httpStatus', 'createRoleAliasResponse_httpStatus' - The response's http status code.
newCreateRoleAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRoleAliasResponse
newCreateRoleAliasResponse pHttpStatus_ =
  CreateRoleAliasResponse'
    { roleAlias =
        Prelude.Nothing,
      roleAliasArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The role alias.
createRoleAliasResponse_roleAlias :: Lens.Lens' CreateRoleAliasResponse (Prelude.Maybe Prelude.Text)
createRoleAliasResponse_roleAlias = Lens.lens (\CreateRoleAliasResponse' {roleAlias} -> roleAlias) (\s@CreateRoleAliasResponse' {} a -> s {roleAlias = a} :: CreateRoleAliasResponse)

-- | The role alias ARN.
createRoleAliasResponse_roleAliasArn :: Lens.Lens' CreateRoleAliasResponse (Prelude.Maybe Prelude.Text)
createRoleAliasResponse_roleAliasArn = Lens.lens (\CreateRoleAliasResponse' {roleAliasArn} -> roleAliasArn) (\s@CreateRoleAliasResponse' {} a -> s {roleAliasArn = a} :: CreateRoleAliasResponse)

-- | The response's http status code.
createRoleAliasResponse_httpStatus :: Lens.Lens' CreateRoleAliasResponse Prelude.Int
createRoleAliasResponse_httpStatus = Lens.lens (\CreateRoleAliasResponse' {httpStatus} -> httpStatus) (\s@CreateRoleAliasResponse' {} a -> s {httpStatus = a} :: CreateRoleAliasResponse)

instance Prelude.NFData CreateRoleAliasResponse where
  rnf CreateRoleAliasResponse' {..} =
    Prelude.rnf roleAlias
      `Prelude.seq` Prelude.rnf roleAliasArn
      `Prelude.seq` Prelude.rnf httpStatus
