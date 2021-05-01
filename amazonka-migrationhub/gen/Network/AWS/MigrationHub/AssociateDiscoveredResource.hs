{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MigrationHub.AssociateDiscoveredResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a discovered resource ID from Application Discovery Service
-- with a migration task.
module Network.AWS.MigrationHub.AssociateDiscoveredResource
  ( -- * Creating a Request
    AssociateDiscoveredResource (..),
    newAssociateDiscoveredResource,

    -- * Request Lenses
    associateDiscoveredResource_dryRun,
    associateDiscoveredResource_progressUpdateStream,
    associateDiscoveredResource_migrationTaskName,
    associateDiscoveredResource_discoveredResource,

    -- * Destructuring the Response
    AssociateDiscoveredResourceResponse (..),
    newAssociateDiscoveredResourceResponse,

    -- * Response Lenses
    associateDiscoveredResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateDiscoveredResource' smart constructor.
data AssociateDiscoveredResource = AssociateDiscoveredResource'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | The identifier given to the MigrationTask. /Do not store personal data
    -- in this field./
    migrationTaskName :: Prelude.Text,
    -- | Object representing a Resource.
    discoveredResource :: DiscoveredResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateDiscoveredResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateDiscoveredResource_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStream', 'associateDiscoveredResource_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'associateDiscoveredResource_migrationTaskName' - The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
--
-- 'discoveredResource', 'associateDiscoveredResource_discoveredResource' - Object representing a Resource.
newAssociateDiscoveredResource ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  -- | 'discoveredResource'
  DiscoveredResource ->
  AssociateDiscoveredResource
newAssociateDiscoveredResource
  pProgressUpdateStream_
  pMigrationTaskName_
  pDiscoveredResource_ =
    AssociateDiscoveredResource'
      { dryRun =
          Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        discoveredResource = pDiscoveredResource_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
associateDiscoveredResource_dryRun :: Lens.Lens' AssociateDiscoveredResource (Prelude.Maybe Prelude.Bool)
associateDiscoveredResource_dryRun = Lens.lens (\AssociateDiscoveredResource' {dryRun} -> dryRun) (\s@AssociateDiscoveredResource' {} a -> s {dryRun = a} :: AssociateDiscoveredResource)

-- | The name of the ProgressUpdateStream.
associateDiscoveredResource_progressUpdateStream :: Lens.Lens' AssociateDiscoveredResource Prelude.Text
associateDiscoveredResource_progressUpdateStream = Lens.lens (\AssociateDiscoveredResource' {progressUpdateStream} -> progressUpdateStream) (\s@AssociateDiscoveredResource' {} a -> s {progressUpdateStream = a} :: AssociateDiscoveredResource)

-- | The identifier given to the MigrationTask. /Do not store personal data
-- in this field./
associateDiscoveredResource_migrationTaskName :: Lens.Lens' AssociateDiscoveredResource Prelude.Text
associateDiscoveredResource_migrationTaskName = Lens.lens (\AssociateDiscoveredResource' {migrationTaskName} -> migrationTaskName) (\s@AssociateDiscoveredResource' {} a -> s {migrationTaskName = a} :: AssociateDiscoveredResource)

-- | Object representing a Resource.
associateDiscoveredResource_discoveredResource :: Lens.Lens' AssociateDiscoveredResource DiscoveredResource
associateDiscoveredResource_discoveredResource = Lens.lens (\AssociateDiscoveredResource' {discoveredResource} -> discoveredResource) (\s@AssociateDiscoveredResource' {} a -> s {discoveredResource = a} :: AssociateDiscoveredResource)

instance
  Prelude.AWSRequest
    AssociateDiscoveredResource
  where
  type
    Rs AssociateDiscoveredResource =
      AssociateDiscoveredResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDiscoveredResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDiscoveredResource

instance Prelude.NFData AssociateDiscoveredResource

instance
  Prelude.ToHeaders
    AssociateDiscoveredResource
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.AssociateDiscoveredResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateDiscoveredResource where
  toJSON AssociateDiscoveredResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DryRun" Prelude..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Prelude..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Prelude..= migrationTaskName),
            Prelude.Just
              ( "DiscoveredResource"
                  Prelude..= discoveredResource
              )
          ]
      )

instance Prelude.ToPath AssociateDiscoveredResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateDiscoveredResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDiscoveredResourceResponse' smart constructor.
data AssociateDiscoveredResourceResponse = AssociateDiscoveredResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateDiscoveredResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDiscoveredResourceResponse_httpStatus' - The response's http status code.
newAssociateDiscoveredResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDiscoveredResourceResponse
newAssociateDiscoveredResourceResponse pHttpStatus_ =
  AssociateDiscoveredResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDiscoveredResourceResponse_httpStatus :: Lens.Lens' AssociateDiscoveredResourceResponse Prelude.Int
associateDiscoveredResourceResponse_httpStatus = Lens.lens (\AssociateDiscoveredResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateDiscoveredResourceResponse' {} a -> s {httpStatus = a} :: AssociateDiscoveredResourceResponse)

instance
  Prelude.NFData
    AssociateDiscoveredResourceResponse
