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
-- Module      : Network.AWS.CloudDirectory.UpgradeAppliedSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a single directory in-place using the @PublishedSchemaArn@ with
-- schema updates found in @MinorVersion@. Backwards-compatible minor
-- version upgrades are instantaneously available for readers on all
-- objects in the directory. Note: This is a synchronous API call and
-- upgrades only one schema on a given directory per call. To upgrade
-- multiple directories from one schema, you would need to call this API on
-- each directory.
module Network.AWS.CloudDirectory.UpgradeAppliedSchema
  ( -- * Creating a Request
    UpgradeAppliedSchema (..),
    newUpgradeAppliedSchema,

    -- * Request Lenses
    upgradeAppliedSchema_dryRun,
    upgradeAppliedSchema_publishedSchemaArn,
    upgradeAppliedSchema_directoryArn,

    -- * Destructuring the Response
    UpgradeAppliedSchemaResponse (..),
    newUpgradeAppliedSchemaResponse,

    -- * Response Lenses
    upgradeAppliedSchemaResponse_directoryArn,
    upgradeAppliedSchemaResponse_upgradedSchemaArn,
    upgradeAppliedSchemaResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpgradeAppliedSchema' smart constructor.
data UpgradeAppliedSchema = UpgradeAppliedSchema'
  { -- | Used for testing whether the major version schemas are backward
    -- compatible or not. If schema compatibility fails, an exception would be
    -- thrown else the call would succeed but no changes will be saved. This
    -- parameter is optional.
    dryRun :: Core.Maybe Core.Bool,
    -- | The revision of the published schema to upgrade the directory to.
    publishedSchemaArn :: Core.Text,
    -- | The ARN for the directory to which the upgraded schema will be applied.
    directoryArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpgradeAppliedSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'upgradeAppliedSchema_dryRun' - Used for testing whether the major version schemas are backward
-- compatible or not. If schema compatibility fails, an exception would be
-- thrown else the call would succeed but no changes will be saved. This
-- parameter is optional.
--
-- 'publishedSchemaArn', 'upgradeAppliedSchema_publishedSchemaArn' - The revision of the published schema to upgrade the directory to.
--
-- 'directoryArn', 'upgradeAppliedSchema_directoryArn' - The ARN for the directory to which the upgraded schema will be applied.
newUpgradeAppliedSchema ::
  -- | 'publishedSchemaArn'
  Core.Text ->
  -- | 'directoryArn'
  Core.Text ->
  UpgradeAppliedSchema
newUpgradeAppliedSchema
  pPublishedSchemaArn_
  pDirectoryArn_ =
    UpgradeAppliedSchema'
      { dryRun = Core.Nothing,
        publishedSchemaArn = pPublishedSchemaArn_,
        directoryArn = pDirectoryArn_
      }

-- | Used for testing whether the major version schemas are backward
-- compatible or not. If schema compatibility fails, an exception would be
-- thrown else the call would succeed but no changes will be saved. This
-- parameter is optional.
upgradeAppliedSchema_dryRun :: Lens.Lens' UpgradeAppliedSchema (Core.Maybe Core.Bool)
upgradeAppliedSchema_dryRun = Lens.lens (\UpgradeAppliedSchema' {dryRun} -> dryRun) (\s@UpgradeAppliedSchema' {} a -> s {dryRun = a} :: UpgradeAppliedSchema)

-- | The revision of the published schema to upgrade the directory to.
upgradeAppliedSchema_publishedSchemaArn :: Lens.Lens' UpgradeAppliedSchema Core.Text
upgradeAppliedSchema_publishedSchemaArn = Lens.lens (\UpgradeAppliedSchema' {publishedSchemaArn} -> publishedSchemaArn) (\s@UpgradeAppliedSchema' {} a -> s {publishedSchemaArn = a} :: UpgradeAppliedSchema)

-- | The ARN for the directory to which the upgraded schema will be applied.
upgradeAppliedSchema_directoryArn :: Lens.Lens' UpgradeAppliedSchema Core.Text
upgradeAppliedSchema_directoryArn = Lens.lens (\UpgradeAppliedSchema' {directoryArn} -> directoryArn) (\s@UpgradeAppliedSchema' {} a -> s {directoryArn = a} :: UpgradeAppliedSchema)

instance Core.AWSRequest UpgradeAppliedSchema where
  type
    AWSResponse UpgradeAppliedSchema =
      UpgradeAppliedSchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeAppliedSchemaResponse'
            Core.<$> (x Core..?> "DirectoryArn")
            Core.<*> (x Core..?> "UpgradedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpgradeAppliedSchema

instance Core.NFData UpgradeAppliedSchema

instance Core.ToHeaders UpgradeAppliedSchema where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpgradeAppliedSchema where
  toJSON UpgradeAppliedSchema' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            Core.Just
              ("PublishedSchemaArn" Core..= publishedSchemaArn),
            Core.Just ("DirectoryArn" Core..= directoryArn)
          ]
      )

instance Core.ToPath UpgradeAppliedSchema where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/upgradeapplied"

instance Core.ToQuery UpgradeAppliedSchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpgradeAppliedSchemaResponse' smart constructor.
data UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse'
  { -- | The ARN of the directory that is returned as part of the response.
    directoryArn :: Core.Maybe Core.Text,
    -- | The ARN of the upgraded schema that is returned as part of the response.
    upgradedSchemaArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpgradeAppliedSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'upgradeAppliedSchemaResponse_directoryArn' - The ARN of the directory that is returned as part of the response.
--
-- 'upgradedSchemaArn', 'upgradeAppliedSchemaResponse_upgradedSchemaArn' - The ARN of the upgraded schema that is returned as part of the response.
--
-- 'httpStatus', 'upgradeAppliedSchemaResponse_httpStatus' - The response's http status code.
newUpgradeAppliedSchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpgradeAppliedSchemaResponse
newUpgradeAppliedSchemaResponse pHttpStatus_ =
  UpgradeAppliedSchemaResponse'
    { directoryArn =
        Core.Nothing,
      upgradedSchemaArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the directory that is returned as part of the response.
upgradeAppliedSchemaResponse_directoryArn :: Lens.Lens' UpgradeAppliedSchemaResponse (Core.Maybe Core.Text)
upgradeAppliedSchemaResponse_directoryArn = Lens.lens (\UpgradeAppliedSchemaResponse' {directoryArn} -> directoryArn) (\s@UpgradeAppliedSchemaResponse' {} a -> s {directoryArn = a} :: UpgradeAppliedSchemaResponse)

-- | The ARN of the upgraded schema that is returned as part of the response.
upgradeAppliedSchemaResponse_upgradedSchemaArn :: Lens.Lens' UpgradeAppliedSchemaResponse (Core.Maybe Core.Text)
upgradeAppliedSchemaResponse_upgradedSchemaArn = Lens.lens (\UpgradeAppliedSchemaResponse' {upgradedSchemaArn} -> upgradedSchemaArn) (\s@UpgradeAppliedSchemaResponse' {} a -> s {upgradedSchemaArn = a} :: UpgradeAppliedSchemaResponse)

-- | The response's http status code.
upgradeAppliedSchemaResponse_httpStatus :: Lens.Lens' UpgradeAppliedSchemaResponse Core.Int
upgradeAppliedSchemaResponse_httpStatus = Lens.lens (\UpgradeAppliedSchemaResponse' {httpStatus} -> httpStatus) (\s@UpgradeAppliedSchemaResponse' {} a -> s {httpStatus = a} :: UpgradeAppliedSchemaResponse)

instance Core.NFData UpgradeAppliedSchemaResponse
