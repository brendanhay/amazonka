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
-- Module      : Amazonka.MigrationHub.DisassociateDiscoveredResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate an Application Discovery Service discovered resource from a
-- migration task.
module Amazonka.MigrationHub.DisassociateDiscoveredResource
  ( -- * Creating a Request
    DisassociateDiscoveredResource (..),
    newDisassociateDiscoveredResource,

    -- * Request Lenses
    disassociateDiscoveredResource_dryRun,
    disassociateDiscoveredResource_progressUpdateStream,
    disassociateDiscoveredResource_migrationTaskName,
    disassociateDiscoveredResource_configurationId,

    -- * Destructuring the Response
    DisassociateDiscoveredResourceResponse (..),
    newDisassociateDiscoveredResourceResponse,

    -- * Response Lenses
    disassociateDiscoveredResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateDiscoveredResource' smart constructor.
data DisassociateDiscoveredResource = DisassociateDiscoveredResource'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | The identifier given to the MigrationTask. /Do not store personal data
    -- in this field./
    migrationTaskName :: Prelude.Text,
    -- | ConfigurationId of the Application Discovery Service resource to be
    -- disassociated.
    configurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDiscoveredResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disassociateDiscoveredResource_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'disassociateDiscoveredResource_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'disassociateDiscoveredResource_migrationTaskName' - The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
--
-- 'configurationId', 'disassociateDiscoveredResource_configurationId' - ConfigurationId of the Application Discovery Service resource to be
-- disassociated.
newDisassociateDiscoveredResource ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  -- | 'configurationId'
  Prelude.Text ->
  DisassociateDiscoveredResource
newDisassociateDiscoveredResource
  pProgressUpdateStream_
  pMigrationTaskName_
  pConfigurationId_ =
    DisassociateDiscoveredResource'
      { dryRun =
          Prelude.Nothing,
        progressUpdateStream =
          pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        configurationId = pConfigurationId_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
disassociateDiscoveredResource_dryRun :: Lens.Lens' DisassociateDiscoveredResource (Prelude.Maybe Prelude.Bool)
disassociateDiscoveredResource_dryRun = Lens.lens (\DisassociateDiscoveredResource' {dryRun} -> dryRun) (\s@DisassociateDiscoveredResource' {} a -> s {dryRun = a} :: DisassociateDiscoveredResource)

-- | The name of the ProgressUpdateStream.
disassociateDiscoveredResource_progressUpdateStream :: Lens.Lens' DisassociateDiscoveredResource Prelude.Text
disassociateDiscoveredResource_progressUpdateStream = Lens.lens (\DisassociateDiscoveredResource' {progressUpdateStream} -> progressUpdateStream) (\s@DisassociateDiscoveredResource' {} a -> s {progressUpdateStream = a} :: DisassociateDiscoveredResource)

-- | The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
disassociateDiscoveredResource_migrationTaskName :: Lens.Lens' DisassociateDiscoveredResource Prelude.Text
disassociateDiscoveredResource_migrationTaskName = Lens.lens (\DisassociateDiscoveredResource' {migrationTaskName} -> migrationTaskName) (\s@DisassociateDiscoveredResource' {} a -> s {migrationTaskName = a} :: DisassociateDiscoveredResource)

-- | ConfigurationId of the Application Discovery Service resource to be
-- disassociated.
disassociateDiscoveredResource_configurationId :: Lens.Lens' DisassociateDiscoveredResource Prelude.Text
disassociateDiscoveredResource_configurationId = Lens.lens (\DisassociateDiscoveredResource' {configurationId} -> configurationId) (\s@DisassociateDiscoveredResource' {} a -> s {configurationId = a} :: DisassociateDiscoveredResource)

instance
  Core.AWSRequest
    DisassociateDiscoveredResource
  where
  type
    AWSResponse DisassociateDiscoveredResource =
      DisassociateDiscoveredResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDiscoveredResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateDiscoveredResource
  where
  hashWithSalt
    _salt
    DisassociateDiscoveredResource' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` progressUpdateStream
        `Prelude.hashWithSalt` migrationTaskName
        `Prelude.hashWithSalt` configurationId

instance
  Prelude.NFData
    DisassociateDiscoveredResource
  where
  rnf DisassociateDiscoveredResource' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName
      `Prelude.seq` Prelude.rnf configurationId

instance
  Data.ToHeaders
    DisassociateDiscoveredResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.DisassociateDiscoveredResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateDiscoveredResource where
  toJSON DisassociateDiscoveredResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Data..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Data..= migrationTaskName),
            Prelude.Just
              ("ConfigurationId" Data..= configurationId)
          ]
      )

instance Data.ToPath DisassociateDiscoveredResource where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateDiscoveredResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDiscoveredResourceResponse' smart constructor.
data DisassociateDiscoveredResourceResponse = DisassociateDiscoveredResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDiscoveredResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDiscoveredResourceResponse_httpStatus' - The response's http status code.
newDisassociateDiscoveredResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateDiscoveredResourceResponse
newDisassociateDiscoveredResourceResponse
  pHttpStatus_ =
    DisassociateDiscoveredResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateDiscoveredResourceResponse_httpStatus :: Lens.Lens' DisassociateDiscoveredResourceResponse Prelude.Int
disassociateDiscoveredResourceResponse_httpStatus = Lens.lens (\DisassociateDiscoveredResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateDiscoveredResourceResponse' {} a -> s {httpStatus = a} :: DisassociateDiscoveredResourceResponse)

instance
  Prelude.NFData
    DisassociateDiscoveredResourceResponse
  where
  rnf DisassociateDiscoveredResourceResponse' {..} =
    Prelude.rnf httpStatus
