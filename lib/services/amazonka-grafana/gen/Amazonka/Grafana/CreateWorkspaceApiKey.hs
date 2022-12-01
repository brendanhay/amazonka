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
-- Module      : Amazonka.Grafana.CreateWorkspaceApiKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Grafana API key for the workspace. This key can be used to
-- authenticate requests sent to the workspace\'s HTTP API. See
-- <https://docs.aws.amazon.com/grafana/latest/userguide/Using-Grafana-APIs.html>
-- for available APIs and example requests.
module Amazonka.Grafana.CreateWorkspaceApiKey
  ( -- * Creating a Request
    CreateWorkspaceApiKey (..),
    newCreateWorkspaceApiKey,

    -- * Request Lenses
    createWorkspaceApiKey_keyName,
    createWorkspaceApiKey_keyRole,
    createWorkspaceApiKey_secondsToLive,
    createWorkspaceApiKey_workspaceId,

    -- * Destructuring the Response
    CreateWorkspaceApiKeyResponse (..),
    newCreateWorkspaceApiKeyResponse,

    -- * Response Lenses
    createWorkspaceApiKeyResponse_httpStatus,
    createWorkspaceApiKeyResponse_key,
    createWorkspaceApiKeyResponse_keyName,
    createWorkspaceApiKeyResponse_workspaceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkspaceApiKey' smart constructor.
data CreateWorkspaceApiKey = CreateWorkspaceApiKey'
  { -- | Specifies the name of the key. Keynames must be unique to the workspace.
    keyName :: Prelude.Text,
    -- | Specifies the permission level of the key.
    --
    -- Valid values: @VIEWER@|@EDITOR@|@ADMIN@
    keyRole :: Prelude.Text,
    -- | Specifies the time in seconds until the key expires. Keys can be valid
    -- for up to 30 days.
    secondsToLive :: Prelude.Natural,
    -- | The ID of the workspace to create an API key.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'createWorkspaceApiKey_keyName' - Specifies the name of the key. Keynames must be unique to the workspace.
--
-- 'keyRole', 'createWorkspaceApiKey_keyRole' - Specifies the permission level of the key.
--
-- Valid values: @VIEWER@|@EDITOR@|@ADMIN@
--
-- 'secondsToLive', 'createWorkspaceApiKey_secondsToLive' - Specifies the time in seconds until the key expires. Keys can be valid
-- for up to 30 days.
--
-- 'workspaceId', 'createWorkspaceApiKey_workspaceId' - The ID of the workspace to create an API key.
newCreateWorkspaceApiKey ::
  -- | 'keyName'
  Prelude.Text ->
  -- | 'keyRole'
  Prelude.Text ->
  -- | 'secondsToLive'
  Prelude.Natural ->
  -- | 'workspaceId'
  Prelude.Text ->
  CreateWorkspaceApiKey
newCreateWorkspaceApiKey
  pKeyName_
  pKeyRole_
  pSecondsToLive_
  pWorkspaceId_ =
    CreateWorkspaceApiKey'
      { keyName = pKeyName_,
        keyRole = pKeyRole_,
        secondsToLive = pSecondsToLive_,
        workspaceId = pWorkspaceId_
      }

-- | Specifies the name of the key. Keynames must be unique to the workspace.
createWorkspaceApiKey_keyName :: Lens.Lens' CreateWorkspaceApiKey Prelude.Text
createWorkspaceApiKey_keyName = Lens.lens (\CreateWorkspaceApiKey' {keyName} -> keyName) (\s@CreateWorkspaceApiKey' {} a -> s {keyName = a} :: CreateWorkspaceApiKey)

-- | Specifies the permission level of the key.
--
-- Valid values: @VIEWER@|@EDITOR@|@ADMIN@
createWorkspaceApiKey_keyRole :: Lens.Lens' CreateWorkspaceApiKey Prelude.Text
createWorkspaceApiKey_keyRole = Lens.lens (\CreateWorkspaceApiKey' {keyRole} -> keyRole) (\s@CreateWorkspaceApiKey' {} a -> s {keyRole = a} :: CreateWorkspaceApiKey)

-- | Specifies the time in seconds until the key expires. Keys can be valid
-- for up to 30 days.
createWorkspaceApiKey_secondsToLive :: Lens.Lens' CreateWorkspaceApiKey Prelude.Natural
createWorkspaceApiKey_secondsToLive = Lens.lens (\CreateWorkspaceApiKey' {secondsToLive} -> secondsToLive) (\s@CreateWorkspaceApiKey' {} a -> s {secondsToLive = a} :: CreateWorkspaceApiKey)

-- | The ID of the workspace to create an API key.
createWorkspaceApiKey_workspaceId :: Lens.Lens' CreateWorkspaceApiKey Prelude.Text
createWorkspaceApiKey_workspaceId = Lens.lens (\CreateWorkspaceApiKey' {workspaceId} -> workspaceId) (\s@CreateWorkspaceApiKey' {} a -> s {workspaceId = a} :: CreateWorkspaceApiKey)

instance Core.AWSRequest CreateWorkspaceApiKey where
  type
    AWSResponse CreateWorkspaceApiKey =
      CreateWorkspaceApiKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspaceApiKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "key")
            Prelude.<*> (x Core..:> "keyName")
            Prelude.<*> (x Core..:> "workspaceId")
      )

instance Prelude.Hashable CreateWorkspaceApiKey where
  hashWithSalt _salt CreateWorkspaceApiKey' {..} =
    _salt `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` keyRole
      `Prelude.hashWithSalt` secondsToLive
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData CreateWorkspaceApiKey where
  rnf CreateWorkspaceApiKey' {..} =
    Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf keyRole
      `Prelude.seq` Prelude.rnf secondsToLive
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders CreateWorkspaceApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorkspaceApiKey where
  toJSON CreateWorkspaceApiKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("keyName" Core..= keyName),
            Prelude.Just ("keyRole" Core..= keyRole),
            Prelude.Just
              ("secondsToLive" Core..= secondsToLive)
          ]
      )

instance Core.ToPath CreateWorkspaceApiKey where
  toPath CreateWorkspaceApiKey' {..} =
    Prelude.mconcat
      ["/workspaces/", Core.toBS workspaceId, "/apikeys"]

instance Core.ToQuery CreateWorkspaceApiKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkspaceApiKeyResponse' smart constructor.
data CreateWorkspaceApiKeyResponse = CreateWorkspaceApiKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The key token. Use this value as a bearer token to authenticate HTTP
    -- requests to the workspace.
    key :: Core.Sensitive Prelude.Text,
    -- | The name of the key that was created.
    keyName :: Prelude.Text,
    -- | The ID of the workspace that the key is valid for.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkspaceApiKeyResponse_httpStatus' - The response's http status code.
--
-- 'key', 'createWorkspaceApiKeyResponse_key' - The key token. Use this value as a bearer token to authenticate HTTP
-- requests to the workspace.
--
-- 'keyName', 'createWorkspaceApiKeyResponse_keyName' - The name of the key that was created.
--
-- 'workspaceId', 'createWorkspaceApiKeyResponse_workspaceId' - The ID of the workspace that the key is valid for.
newCreateWorkspaceApiKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'key'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  CreateWorkspaceApiKeyResponse
newCreateWorkspaceApiKeyResponse
  pHttpStatus_
  pKey_
  pKeyName_
  pWorkspaceId_ =
    CreateWorkspaceApiKeyResponse'
      { httpStatus =
          pHttpStatus_,
        key = Core._Sensitive Lens.# pKey_,
        keyName = pKeyName_,
        workspaceId = pWorkspaceId_
      }

-- | The response's http status code.
createWorkspaceApiKeyResponse_httpStatus :: Lens.Lens' CreateWorkspaceApiKeyResponse Prelude.Int
createWorkspaceApiKeyResponse_httpStatus = Lens.lens (\CreateWorkspaceApiKeyResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspaceApiKeyResponse' {} a -> s {httpStatus = a} :: CreateWorkspaceApiKeyResponse)

-- | The key token. Use this value as a bearer token to authenticate HTTP
-- requests to the workspace.
createWorkspaceApiKeyResponse_key :: Lens.Lens' CreateWorkspaceApiKeyResponse Prelude.Text
createWorkspaceApiKeyResponse_key = Lens.lens (\CreateWorkspaceApiKeyResponse' {key} -> key) (\s@CreateWorkspaceApiKeyResponse' {} a -> s {key = a} :: CreateWorkspaceApiKeyResponse) Prelude.. Core._Sensitive

-- | The name of the key that was created.
createWorkspaceApiKeyResponse_keyName :: Lens.Lens' CreateWorkspaceApiKeyResponse Prelude.Text
createWorkspaceApiKeyResponse_keyName = Lens.lens (\CreateWorkspaceApiKeyResponse' {keyName} -> keyName) (\s@CreateWorkspaceApiKeyResponse' {} a -> s {keyName = a} :: CreateWorkspaceApiKeyResponse)

-- | The ID of the workspace that the key is valid for.
createWorkspaceApiKeyResponse_workspaceId :: Lens.Lens' CreateWorkspaceApiKeyResponse Prelude.Text
createWorkspaceApiKeyResponse_workspaceId = Lens.lens (\CreateWorkspaceApiKeyResponse' {workspaceId} -> workspaceId) (\s@CreateWorkspaceApiKeyResponse' {} a -> s {workspaceId = a} :: CreateWorkspaceApiKeyResponse)

instance Prelude.NFData CreateWorkspaceApiKeyResponse where
  rnf CreateWorkspaceApiKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf workspaceId
