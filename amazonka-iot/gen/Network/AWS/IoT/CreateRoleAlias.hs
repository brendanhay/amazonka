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
-- Module      : Network.AWS.IoT.CreateRoleAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a role alias.
module Network.AWS.IoT.CreateRoleAlias
  ( -- * Creating a Request
    CreateRoleAlias (..),
    newCreateRoleAlias,

    -- * Request Lenses
    createRoleAlias_tags,
    createRoleAlias_credentialDurationSeconds,
    createRoleAlias_roleAlias,
    createRoleAlias_roleArn,

    -- * Destructuring the Response
    CreateRoleAliasResponse (..),
    newCreateRoleAliasResponse,

    -- * Response Lenses
    createRoleAliasResponse_roleAliasArn,
    createRoleAliasResponse_roleAlias,
    createRoleAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRoleAlias' smart constructor.
data CreateRoleAlias = CreateRoleAlias'
  { -- | Metadata which can be used to manage the role alias.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Core.Maybe [Tag],
    -- | How long (in seconds) the credentials will be valid.
    credentialDurationSeconds :: Core.Maybe Core.Natural,
    -- | The role alias that points to a role ARN. This allows you to change the
    -- role without having to update the device.
    roleAlias :: Core.Text,
    -- | The role ARN.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRoleAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'credentialDurationSeconds', 'createRoleAlias_credentialDurationSeconds' - How long (in seconds) the credentials will be valid.
--
-- 'roleAlias', 'createRoleAlias_roleAlias' - The role alias that points to a role ARN. This allows you to change the
-- role without having to update the device.
--
-- 'roleArn', 'createRoleAlias_roleArn' - The role ARN.
newCreateRoleAlias ::
  -- | 'roleAlias'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  CreateRoleAlias
newCreateRoleAlias pRoleAlias_ pRoleArn_ =
  CreateRoleAlias'
    { tags = Core.Nothing,
      credentialDurationSeconds = Core.Nothing,
      roleAlias = pRoleAlias_,
      roleArn = pRoleArn_
    }

-- | Metadata which can be used to manage the role alias.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createRoleAlias_tags :: Lens.Lens' CreateRoleAlias (Core.Maybe [Tag])
createRoleAlias_tags = Lens.lens (\CreateRoleAlias' {tags} -> tags) (\s@CreateRoleAlias' {} a -> s {tags = a} :: CreateRoleAlias) Core.. Lens.mapping Lens._Coerce

-- | How long (in seconds) the credentials will be valid.
createRoleAlias_credentialDurationSeconds :: Lens.Lens' CreateRoleAlias (Core.Maybe Core.Natural)
createRoleAlias_credentialDurationSeconds = Lens.lens (\CreateRoleAlias' {credentialDurationSeconds} -> credentialDurationSeconds) (\s@CreateRoleAlias' {} a -> s {credentialDurationSeconds = a} :: CreateRoleAlias)

-- | The role alias that points to a role ARN. This allows you to change the
-- role without having to update the device.
createRoleAlias_roleAlias :: Lens.Lens' CreateRoleAlias Core.Text
createRoleAlias_roleAlias = Lens.lens (\CreateRoleAlias' {roleAlias} -> roleAlias) (\s@CreateRoleAlias' {} a -> s {roleAlias = a} :: CreateRoleAlias)

-- | The role ARN.
createRoleAlias_roleArn :: Lens.Lens' CreateRoleAlias Core.Text
createRoleAlias_roleArn = Lens.lens (\CreateRoleAlias' {roleArn} -> roleArn) (\s@CreateRoleAlias' {} a -> s {roleArn = a} :: CreateRoleAlias)

instance Core.AWSRequest CreateRoleAlias where
  type
    AWSResponse CreateRoleAlias =
      CreateRoleAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoleAliasResponse'
            Core.<$> (x Core..?> "roleAliasArn")
            Core.<*> (x Core..?> "roleAlias")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRoleAlias

instance Core.NFData CreateRoleAlias

instance Core.ToHeaders CreateRoleAlias where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateRoleAlias where
  toJSON CreateRoleAlias' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("credentialDurationSeconds" Core..=)
              Core.<$> credentialDurationSeconds,
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateRoleAlias where
  toPath CreateRoleAlias' {..} =
    Core.mconcat
      ["/role-aliases/", Core.toBS roleAlias]

instance Core.ToQuery CreateRoleAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRoleAliasResponse' smart constructor.
data CreateRoleAliasResponse = CreateRoleAliasResponse'
  { -- | The role alias ARN.
    roleAliasArn :: Core.Maybe Core.Text,
    -- | The role alias.
    roleAlias :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRoleAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAliasArn', 'createRoleAliasResponse_roleAliasArn' - The role alias ARN.
--
-- 'roleAlias', 'createRoleAliasResponse_roleAlias' - The role alias.
--
-- 'httpStatus', 'createRoleAliasResponse_httpStatus' - The response's http status code.
newCreateRoleAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRoleAliasResponse
newCreateRoleAliasResponse pHttpStatus_ =
  CreateRoleAliasResponse'
    { roleAliasArn =
        Core.Nothing,
      roleAlias = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The role alias ARN.
createRoleAliasResponse_roleAliasArn :: Lens.Lens' CreateRoleAliasResponse (Core.Maybe Core.Text)
createRoleAliasResponse_roleAliasArn = Lens.lens (\CreateRoleAliasResponse' {roleAliasArn} -> roleAliasArn) (\s@CreateRoleAliasResponse' {} a -> s {roleAliasArn = a} :: CreateRoleAliasResponse)

-- | The role alias.
createRoleAliasResponse_roleAlias :: Lens.Lens' CreateRoleAliasResponse (Core.Maybe Core.Text)
createRoleAliasResponse_roleAlias = Lens.lens (\CreateRoleAliasResponse' {roleAlias} -> roleAlias) (\s@CreateRoleAliasResponse' {} a -> s {roleAlias = a} :: CreateRoleAliasResponse)

-- | The response's http status code.
createRoleAliasResponse_httpStatus :: Lens.Lens' CreateRoleAliasResponse Core.Int
createRoleAliasResponse_httpStatus = Lens.lens (\CreateRoleAliasResponse' {httpStatus} -> httpStatus) (\s@CreateRoleAliasResponse' {} a -> s {httpStatus = a} :: CreateRoleAliasResponse)

instance Core.NFData CreateRoleAliasResponse
