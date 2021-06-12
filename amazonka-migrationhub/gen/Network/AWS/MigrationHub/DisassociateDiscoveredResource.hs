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
-- Module      : Network.AWS.MigrationHub.DisassociateDiscoveredResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate an Application Discovery Service discovered resource from a
-- migration task.
module Network.AWS.MigrationHub.DisassociateDiscoveredResource
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateDiscoveredResource' smart constructor.
data DisassociateDiscoveredResource = DisassociateDiscoveredResource'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Core.Text,
    -- | The identifier given to the MigrationTask. /Do not store personal data
    -- in this field./
    migrationTaskName :: Core.Text,
    -- | ConfigurationId of the Application Discovery Service resource to be
    -- disassociated.
    configurationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'migrationTaskName'
  Core.Text ->
  -- | 'configurationId'
  Core.Text ->
  DisassociateDiscoveredResource
newDisassociateDiscoveredResource
  pProgressUpdateStream_
  pMigrationTaskName_
  pConfigurationId_ =
    DisassociateDiscoveredResource'
      { dryRun =
          Core.Nothing,
        progressUpdateStream =
          pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        configurationId = pConfigurationId_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
disassociateDiscoveredResource_dryRun :: Lens.Lens' DisassociateDiscoveredResource (Core.Maybe Core.Bool)
disassociateDiscoveredResource_dryRun = Lens.lens (\DisassociateDiscoveredResource' {dryRun} -> dryRun) (\s@DisassociateDiscoveredResource' {} a -> s {dryRun = a} :: DisassociateDiscoveredResource)

-- | The name of the ProgressUpdateStream.
disassociateDiscoveredResource_progressUpdateStream :: Lens.Lens' DisassociateDiscoveredResource Core.Text
disassociateDiscoveredResource_progressUpdateStream = Lens.lens (\DisassociateDiscoveredResource' {progressUpdateStream} -> progressUpdateStream) (\s@DisassociateDiscoveredResource' {} a -> s {progressUpdateStream = a} :: DisassociateDiscoveredResource)

-- | The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
disassociateDiscoveredResource_migrationTaskName :: Lens.Lens' DisassociateDiscoveredResource Core.Text
disassociateDiscoveredResource_migrationTaskName = Lens.lens (\DisassociateDiscoveredResource' {migrationTaskName} -> migrationTaskName) (\s@DisassociateDiscoveredResource' {} a -> s {migrationTaskName = a} :: DisassociateDiscoveredResource)

-- | ConfigurationId of the Application Discovery Service resource to be
-- disassociated.
disassociateDiscoveredResource_configurationId :: Lens.Lens' DisassociateDiscoveredResource Core.Text
disassociateDiscoveredResource_configurationId = Lens.lens (\DisassociateDiscoveredResource' {configurationId} -> configurationId) (\s@DisassociateDiscoveredResource' {} a -> s {configurationId = a} :: DisassociateDiscoveredResource)

instance
  Core.AWSRequest
    DisassociateDiscoveredResource
  where
  type
    AWSResponse DisassociateDiscoveredResource =
      DisassociateDiscoveredResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDiscoveredResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateDiscoveredResource

instance Core.NFData DisassociateDiscoveredResource

instance
  Core.ToHeaders
    DisassociateDiscoveredResource
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.DisassociateDiscoveredResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateDiscoveredResource where
  toJSON DisassociateDiscoveredResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            Core.Just
              ( "ProgressUpdateStream"
                  Core..= progressUpdateStream
              ),
            Core.Just
              ("MigrationTaskName" Core..= migrationTaskName),
            Core.Just
              ("ConfigurationId" Core..= configurationId)
          ]
      )

instance Core.ToPath DisassociateDiscoveredResource where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateDiscoveredResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateDiscoveredResourceResponse' smart constructor.
data DisassociateDiscoveredResourceResponse = DisassociateDiscoveredResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateDiscoveredResourceResponse
newDisassociateDiscoveredResourceResponse
  pHttpStatus_ =
    DisassociateDiscoveredResourceResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateDiscoveredResourceResponse_httpStatus :: Lens.Lens' DisassociateDiscoveredResourceResponse Core.Int
disassociateDiscoveredResourceResponse_httpStatus = Lens.lens (\DisassociateDiscoveredResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateDiscoveredResourceResponse' {} a -> s {httpStatus = a} :: DisassociateDiscoveredResourceResponse)

instance
  Core.NFData
    DisassociateDiscoveredResourceResponse
