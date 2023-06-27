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
-- Module      : Amazonka.CloudDirectory.UpgradeAppliedSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudDirectory.UpgradeAppliedSchema
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpgradeAppliedSchema' smart constructor.
data UpgradeAppliedSchema = UpgradeAppliedSchema'
  { -- | Used for testing whether the major version schemas are backward
    -- compatible or not. If schema compatibility fails, an exception would be
    -- thrown else the call would succeed but no changes will be saved. This
    -- parameter is optional.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The revision of the published schema to upgrade the directory to.
    publishedSchemaArn :: Prelude.Text,
    -- | The ARN for the directory to which the upgraded schema will be applied.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'directoryArn'
  Prelude.Text ->
  UpgradeAppliedSchema
newUpgradeAppliedSchema
  pPublishedSchemaArn_
  pDirectoryArn_ =
    UpgradeAppliedSchema'
      { dryRun = Prelude.Nothing,
        publishedSchemaArn = pPublishedSchemaArn_,
        directoryArn = pDirectoryArn_
      }

-- | Used for testing whether the major version schemas are backward
-- compatible or not. If schema compatibility fails, an exception would be
-- thrown else the call would succeed but no changes will be saved. This
-- parameter is optional.
upgradeAppliedSchema_dryRun :: Lens.Lens' UpgradeAppliedSchema (Prelude.Maybe Prelude.Bool)
upgradeAppliedSchema_dryRun = Lens.lens (\UpgradeAppliedSchema' {dryRun} -> dryRun) (\s@UpgradeAppliedSchema' {} a -> s {dryRun = a} :: UpgradeAppliedSchema)

-- | The revision of the published schema to upgrade the directory to.
upgradeAppliedSchema_publishedSchemaArn :: Lens.Lens' UpgradeAppliedSchema Prelude.Text
upgradeAppliedSchema_publishedSchemaArn = Lens.lens (\UpgradeAppliedSchema' {publishedSchemaArn} -> publishedSchemaArn) (\s@UpgradeAppliedSchema' {} a -> s {publishedSchemaArn = a} :: UpgradeAppliedSchema)

-- | The ARN for the directory to which the upgraded schema will be applied.
upgradeAppliedSchema_directoryArn :: Lens.Lens' UpgradeAppliedSchema Prelude.Text
upgradeAppliedSchema_directoryArn = Lens.lens (\UpgradeAppliedSchema' {directoryArn} -> directoryArn) (\s@UpgradeAppliedSchema' {} a -> s {directoryArn = a} :: UpgradeAppliedSchema)

instance Core.AWSRequest UpgradeAppliedSchema where
  type
    AWSResponse UpgradeAppliedSchema =
      UpgradeAppliedSchemaResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeAppliedSchemaResponse'
            Prelude.<$> (x Data..?> "DirectoryArn")
            Prelude.<*> (x Data..?> "UpgradedSchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpgradeAppliedSchema where
  hashWithSalt _salt UpgradeAppliedSchema' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` publishedSchemaArn
      `Prelude.hashWithSalt` directoryArn

instance Prelude.NFData UpgradeAppliedSchema where
  rnf UpgradeAppliedSchema' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf publishedSchemaArn
      `Prelude.seq` Prelude.rnf directoryArn

instance Data.ToHeaders UpgradeAppliedSchema where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpgradeAppliedSchema where
  toJSON UpgradeAppliedSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just
              ("PublishedSchemaArn" Data..= publishedSchemaArn),
            Prelude.Just ("DirectoryArn" Data..= directoryArn)
          ]
      )

instance Data.ToPath UpgradeAppliedSchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/upgradeapplied"

instance Data.ToQuery UpgradeAppliedSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpgradeAppliedSchemaResponse' smart constructor.
data UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse'
  { -- | The ARN of the directory that is returned as part of the response.
    directoryArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the upgraded schema that is returned as part of the response.
    upgradedSchemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpgradeAppliedSchemaResponse
newUpgradeAppliedSchemaResponse pHttpStatus_ =
  UpgradeAppliedSchemaResponse'
    { directoryArn =
        Prelude.Nothing,
      upgradedSchemaArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the directory that is returned as part of the response.
upgradeAppliedSchemaResponse_directoryArn :: Lens.Lens' UpgradeAppliedSchemaResponse (Prelude.Maybe Prelude.Text)
upgradeAppliedSchemaResponse_directoryArn = Lens.lens (\UpgradeAppliedSchemaResponse' {directoryArn} -> directoryArn) (\s@UpgradeAppliedSchemaResponse' {} a -> s {directoryArn = a} :: UpgradeAppliedSchemaResponse)

-- | The ARN of the upgraded schema that is returned as part of the response.
upgradeAppliedSchemaResponse_upgradedSchemaArn :: Lens.Lens' UpgradeAppliedSchemaResponse (Prelude.Maybe Prelude.Text)
upgradeAppliedSchemaResponse_upgradedSchemaArn = Lens.lens (\UpgradeAppliedSchemaResponse' {upgradedSchemaArn} -> upgradedSchemaArn) (\s@UpgradeAppliedSchemaResponse' {} a -> s {upgradedSchemaArn = a} :: UpgradeAppliedSchemaResponse)

-- | The response's http status code.
upgradeAppliedSchemaResponse_httpStatus :: Lens.Lens' UpgradeAppliedSchemaResponse Prelude.Int
upgradeAppliedSchemaResponse_httpStatus = Lens.lens (\UpgradeAppliedSchemaResponse' {httpStatus} -> httpStatus) (\s@UpgradeAppliedSchemaResponse' {} a -> s {httpStatus = a} :: UpgradeAppliedSchemaResponse)

instance Prelude.NFData UpgradeAppliedSchemaResponse where
  rnf UpgradeAppliedSchemaResponse' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf upgradedSchemaArn
      `Prelude.seq` Prelude.rnf httpStatus
