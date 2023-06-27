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
-- Module      : Amazonka.FSx.CreateVolumeFromBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon FSx for NetApp ONTAP volume from an existing Amazon
-- FSx volume backup.
module Amazonka.FSx.CreateVolumeFromBackup
  ( -- * Creating a Request
    CreateVolumeFromBackup (..),
    newCreateVolumeFromBackup,

    -- * Request Lenses
    createVolumeFromBackup_clientRequestToken,
    createVolumeFromBackup_ontapConfiguration,
    createVolumeFromBackup_tags,
    createVolumeFromBackup_backupId,
    createVolumeFromBackup_name,

    -- * Destructuring the Response
    CreateVolumeFromBackupResponse (..),
    newCreateVolumeFromBackupResponse,

    -- * Response Lenses
    createVolumeFromBackupResponse_volume,
    createVolumeFromBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVolumeFromBackup' smart constructor.
data CreateVolumeFromBackup = CreateVolumeFromBackup'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the configuration of the ONTAP volume that you are creating.
    ontapConfiguration :: Prelude.Maybe CreateOntapVolumeConfiguration,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    backupId :: Prelude.Text,
    -- | The name of the new volume you\'re creating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVolumeFromBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createVolumeFromBackup_clientRequestToken' - Undocumented member.
--
-- 'ontapConfiguration', 'createVolumeFromBackup_ontapConfiguration' - Specifies the configuration of the ONTAP volume that you are creating.
--
-- 'tags', 'createVolumeFromBackup_tags' - Undocumented member.
--
-- 'backupId', 'createVolumeFromBackup_backupId' - Undocumented member.
--
-- 'name', 'createVolumeFromBackup_name' - The name of the new volume you\'re creating.
newCreateVolumeFromBackup ::
  -- | 'backupId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateVolumeFromBackup
newCreateVolumeFromBackup pBackupId_ pName_ =
  CreateVolumeFromBackup'
    { clientRequestToken =
        Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      backupId = pBackupId_,
      name = pName_
    }

-- | Undocumented member.
createVolumeFromBackup_clientRequestToken :: Lens.Lens' CreateVolumeFromBackup (Prelude.Maybe Prelude.Text)
createVolumeFromBackup_clientRequestToken = Lens.lens (\CreateVolumeFromBackup' {clientRequestToken} -> clientRequestToken) (\s@CreateVolumeFromBackup' {} a -> s {clientRequestToken = a} :: CreateVolumeFromBackup)

-- | Specifies the configuration of the ONTAP volume that you are creating.
createVolumeFromBackup_ontapConfiguration :: Lens.Lens' CreateVolumeFromBackup (Prelude.Maybe CreateOntapVolumeConfiguration)
createVolumeFromBackup_ontapConfiguration = Lens.lens (\CreateVolumeFromBackup' {ontapConfiguration} -> ontapConfiguration) (\s@CreateVolumeFromBackup' {} a -> s {ontapConfiguration = a} :: CreateVolumeFromBackup)

-- | Undocumented member.
createVolumeFromBackup_tags :: Lens.Lens' CreateVolumeFromBackup (Prelude.Maybe (Prelude.NonEmpty Tag))
createVolumeFromBackup_tags = Lens.lens (\CreateVolumeFromBackup' {tags} -> tags) (\s@CreateVolumeFromBackup' {} a -> s {tags = a} :: CreateVolumeFromBackup) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createVolumeFromBackup_backupId :: Lens.Lens' CreateVolumeFromBackup Prelude.Text
createVolumeFromBackup_backupId = Lens.lens (\CreateVolumeFromBackup' {backupId} -> backupId) (\s@CreateVolumeFromBackup' {} a -> s {backupId = a} :: CreateVolumeFromBackup)

-- | The name of the new volume you\'re creating.
createVolumeFromBackup_name :: Lens.Lens' CreateVolumeFromBackup Prelude.Text
createVolumeFromBackup_name = Lens.lens (\CreateVolumeFromBackup' {name} -> name) (\s@CreateVolumeFromBackup' {} a -> s {name = a} :: CreateVolumeFromBackup)

instance Core.AWSRequest CreateVolumeFromBackup where
  type
    AWSResponse CreateVolumeFromBackup =
      CreateVolumeFromBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVolumeFromBackupResponse'
            Prelude.<$> (x Data..?> "Volume")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVolumeFromBackup where
  hashWithSalt _salt CreateVolumeFromBackup' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateVolumeFromBackup where
  rnf CreateVolumeFromBackup' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf ontapConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateVolumeFromBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateVolumeFromBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVolumeFromBackup where
  toJSON CreateVolumeFromBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("OntapConfiguration" Data..=)
              Prelude.<$> ontapConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("BackupId" Data..= backupId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateVolumeFromBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVolumeFromBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVolumeFromBackupResponse' smart constructor.
data CreateVolumeFromBackupResponse = CreateVolumeFromBackupResponse'
  { -- | Returned after a successful @CreateVolumeFromBackup@ API operation,
    -- describing the volume just created.
    volume :: Prelude.Maybe Volume,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVolumeFromBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volume', 'createVolumeFromBackupResponse_volume' - Returned after a successful @CreateVolumeFromBackup@ API operation,
-- describing the volume just created.
--
-- 'httpStatus', 'createVolumeFromBackupResponse_httpStatus' - The response's http status code.
newCreateVolumeFromBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVolumeFromBackupResponse
newCreateVolumeFromBackupResponse pHttpStatus_ =
  CreateVolumeFromBackupResponse'
    { volume =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returned after a successful @CreateVolumeFromBackup@ API operation,
-- describing the volume just created.
createVolumeFromBackupResponse_volume :: Lens.Lens' CreateVolumeFromBackupResponse (Prelude.Maybe Volume)
createVolumeFromBackupResponse_volume = Lens.lens (\CreateVolumeFromBackupResponse' {volume} -> volume) (\s@CreateVolumeFromBackupResponse' {} a -> s {volume = a} :: CreateVolumeFromBackupResponse)

-- | The response's http status code.
createVolumeFromBackupResponse_httpStatus :: Lens.Lens' CreateVolumeFromBackupResponse Prelude.Int
createVolumeFromBackupResponse_httpStatus = Lens.lens (\CreateVolumeFromBackupResponse' {httpStatus} -> httpStatus) (\s@CreateVolumeFromBackupResponse' {} a -> s {httpStatus = a} :: CreateVolumeFromBackupResponse)

instance
  Prelude.NFData
    CreateVolumeFromBackupResponse
  where
  rnf CreateVolumeFromBackupResponse' {..} =
    Prelude.rnf volume
      `Prelude.seq` Prelude.rnf httpStatus
