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
-- Module      : Amazonka.OpsWorksCM.CreateBackup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application-level backup of a server. While the server is in
-- the @BACKING_UP@ state, the server cannot be changed, and no additional
-- backup can be created.
--
-- Backups can be created for servers in @RUNNING@, @HEALTHY@, and
-- @UNHEALTHY@ states. By default, you can create a maximum of 50 manual
-- backups.
--
-- This operation is asynchronous.
--
-- A @LimitExceededException@ is thrown when the maximum number of manual
-- backups is reached. An @InvalidStateException@ is thrown when the server
-- is not in any of the following states: RUNNING, HEALTHY, or UNHEALTHY. A
-- @ResourceNotFoundException@ is thrown when the server is not found. A
-- @ValidationException@ is thrown when parameters of the request are not
-- valid.
module Amazonka.OpsWorksCM.CreateBackup
  ( -- * Creating a Request
    CreateBackup (..),
    newCreateBackup,

    -- * Request Lenses
    createBackup_tags,
    createBackup_description,
    createBackup_serverName,

    -- * Destructuring the Response
    CreateBackupResponse (..),
    newCreateBackupResponse,

    -- * Response Lenses
    createBackupResponse_backup,
    createBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { -- | A map that contains tag keys and tag values to attach to an AWS
    -- OpsWorks-CM server backup.
    --
    -- -   The key cannot be empty.
    --
    -- -   The key can be a maximum of 127 characters, and can contain only
    --     Unicode letters, numbers, or separators, or the following special
    --     characters: @+ - = . _ : \/@
    --
    -- -   The value can be a maximum 255 characters, and contain only Unicode
    --     letters, numbers, or separators, or the following special
    --     characters: @+ - = . _ : \/@
    --
    -- -   Leading and trailing white spaces are trimmed from both the key and
    --     value.
    --
    -- -   A maximum of 50 user-applied tags is allowed for tag-supported AWS
    --     OpsWorks-CM resources.
    tags :: Prelude.Maybe [Tag],
    -- | A user-defined description of the backup.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the server that you want to back up.
    serverName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createBackup_tags' - A map that contains tag keys and tag values to attach to an AWS
-- OpsWorks-CM server backup.
--
-- -   The key cannot be empty.
--
-- -   The key can be a maximum of 127 characters, and can contain only
--     Unicode letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   The value can be a maximum 255 characters, and contain only Unicode
--     letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   Leading and trailing white spaces are trimmed from both the key and
--     value.
--
-- -   A maximum of 50 user-applied tags is allowed for tag-supported AWS
--     OpsWorks-CM resources.
--
-- 'description', 'createBackup_description' - A user-defined description of the backup.
--
-- 'serverName', 'createBackup_serverName' - The name of the server that you want to back up.
newCreateBackup ::
  -- | 'serverName'
  Prelude.Text ->
  CreateBackup
newCreateBackup pServerName_ =
  CreateBackup'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      serverName = pServerName_
    }

-- | A map that contains tag keys and tag values to attach to an AWS
-- OpsWorks-CM server backup.
--
-- -   The key cannot be empty.
--
-- -   The key can be a maximum of 127 characters, and can contain only
--     Unicode letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   The value can be a maximum 255 characters, and contain only Unicode
--     letters, numbers, or separators, or the following special
--     characters: @+ - = . _ : \/@
--
-- -   Leading and trailing white spaces are trimmed from both the key and
--     value.
--
-- -   A maximum of 50 user-applied tags is allowed for tag-supported AWS
--     OpsWorks-CM resources.
createBackup_tags :: Lens.Lens' CreateBackup (Prelude.Maybe [Tag])
createBackup_tags = Lens.lens (\CreateBackup' {tags} -> tags) (\s@CreateBackup' {} a -> s {tags = a} :: CreateBackup) Prelude.. Lens.mapping Lens.coerced

-- | A user-defined description of the backup.
createBackup_description :: Lens.Lens' CreateBackup (Prelude.Maybe Prelude.Text)
createBackup_description = Lens.lens (\CreateBackup' {description} -> description) (\s@CreateBackup' {} a -> s {description = a} :: CreateBackup)

-- | The name of the server that you want to back up.
createBackup_serverName :: Lens.Lens' CreateBackup Prelude.Text
createBackup_serverName = Lens.lens (\CreateBackup' {serverName} -> serverName) (\s@CreateBackup' {} a -> s {serverName = a} :: CreateBackup)

instance Core.AWSRequest CreateBackup where
  type AWSResponse CreateBackup = CreateBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupResponse'
            Prelude.<$> (x Core..?> "Backup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackup where
  hashWithSalt _salt CreateBackup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData CreateBackup where
  rnf CreateBackup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf serverName

instance Core.ToHeaders CreateBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.CreateBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBackup where
  toJSON CreateBackup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("ServerName" Core..= serverName)
          ]
      )

instance Core.ToPath CreateBackup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { -- | Backup created by request.
    backup :: Prelude.Maybe Backup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backup', 'createBackupResponse_backup' - Backup created by request.
--
-- 'httpStatus', 'createBackupResponse_httpStatus' - The response's http status code.
newCreateBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackupResponse
newCreateBackupResponse pHttpStatus_ =
  CreateBackupResponse'
    { backup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Backup created by request.
createBackupResponse_backup :: Lens.Lens' CreateBackupResponse (Prelude.Maybe Backup)
createBackupResponse_backup = Lens.lens (\CreateBackupResponse' {backup} -> backup) (\s@CreateBackupResponse' {} a -> s {backup = a} :: CreateBackupResponse)

-- | The response's http status code.
createBackupResponse_httpStatus :: Lens.Lens' CreateBackupResponse Prelude.Int
createBackupResponse_httpStatus = Lens.lens (\CreateBackupResponse' {httpStatus} -> httpStatus) (\s@CreateBackupResponse' {} a -> s {httpStatus = a} :: CreateBackupResponse)

instance Prelude.NFData CreateBackupResponse where
  rnf CreateBackupResponse' {..} =
    Prelude.rnf backup
      `Prelude.seq` Prelude.rnf httpStatus
