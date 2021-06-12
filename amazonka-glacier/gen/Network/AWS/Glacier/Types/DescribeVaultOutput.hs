{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DescribeVaultOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DescribeVaultOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newDescribeVaultOutput' smart constructor.
data DescribeVaultOutput = DescribeVaultOutput'
  { -- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier
    -- completed the last vault inventory. This value should be a string in the
    -- ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@.
    lastInventoryDate :: Core.Maybe Core.Text,
    -- | The Universal Coordinated Time (UTC) date when the vault was created.
    -- This value should be a string in the ISO 8601 date format, for example
    -- @2012-03-20T17:03:43.221Z@.
    creationDate :: Core.Maybe Core.Text,
    -- | The name of the vault.
    vaultName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the vault.
    vaultARN :: Core.Maybe Core.Text,
    -- | Total size, in bytes, of the archives in the vault as of the last
    -- inventory date. This field will return null if an inventory has not yet
    -- run on the vault, for example if you just created the vault.
    sizeInBytes :: Core.Maybe Core.Integer,
    -- | The number of archives in the vault as of the last inventory date. This
    -- field will return @null@ if an inventory has not yet run on the vault,
    -- for example if you just created the vault.
    numberOfArchives :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVaultOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastInventoryDate', 'describeVaultOutput_lastInventoryDate' - The Universal Coordinated Time (UTC) date when Amazon S3 Glacier
-- completed the last vault inventory. This value should be a string in the
-- ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@.
--
-- 'creationDate', 'describeVaultOutput_creationDate' - The Universal Coordinated Time (UTC) date when the vault was created.
-- This value should be a string in the ISO 8601 date format, for example
-- @2012-03-20T17:03:43.221Z@.
--
-- 'vaultName', 'describeVaultOutput_vaultName' - The name of the vault.
--
-- 'vaultARN', 'describeVaultOutput_vaultARN' - The Amazon Resource Name (ARN) of the vault.
--
-- 'sizeInBytes', 'describeVaultOutput_sizeInBytes' - Total size, in bytes, of the archives in the vault as of the last
-- inventory date. This field will return null if an inventory has not yet
-- run on the vault, for example if you just created the vault.
--
-- 'numberOfArchives', 'describeVaultOutput_numberOfArchives' - The number of archives in the vault as of the last inventory date. This
-- field will return @null@ if an inventory has not yet run on the vault,
-- for example if you just created the vault.
newDescribeVaultOutput ::
  DescribeVaultOutput
newDescribeVaultOutput =
  DescribeVaultOutput'
    { lastInventoryDate =
        Core.Nothing,
      creationDate = Core.Nothing,
      vaultName = Core.Nothing,
      vaultARN = Core.Nothing,
      sizeInBytes = Core.Nothing,
      numberOfArchives = Core.Nothing
    }

-- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier
-- completed the last vault inventory. This value should be a string in the
-- ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@.
describeVaultOutput_lastInventoryDate :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Text)
describeVaultOutput_lastInventoryDate = Lens.lens (\DescribeVaultOutput' {lastInventoryDate} -> lastInventoryDate) (\s@DescribeVaultOutput' {} a -> s {lastInventoryDate = a} :: DescribeVaultOutput)

-- | The Universal Coordinated Time (UTC) date when the vault was created.
-- This value should be a string in the ISO 8601 date format, for example
-- @2012-03-20T17:03:43.221Z@.
describeVaultOutput_creationDate :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Text)
describeVaultOutput_creationDate = Lens.lens (\DescribeVaultOutput' {creationDate} -> creationDate) (\s@DescribeVaultOutput' {} a -> s {creationDate = a} :: DescribeVaultOutput)

-- | The name of the vault.
describeVaultOutput_vaultName :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Text)
describeVaultOutput_vaultName = Lens.lens (\DescribeVaultOutput' {vaultName} -> vaultName) (\s@DescribeVaultOutput' {} a -> s {vaultName = a} :: DescribeVaultOutput)

-- | The Amazon Resource Name (ARN) of the vault.
describeVaultOutput_vaultARN :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Text)
describeVaultOutput_vaultARN = Lens.lens (\DescribeVaultOutput' {vaultARN} -> vaultARN) (\s@DescribeVaultOutput' {} a -> s {vaultARN = a} :: DescribeVaultOutput)

-- | Total size, in bytes, of the archives in the vault as of the last
-- inventory date. This field will return null if an inventory has not yet
-- run on the vault, for example if you just created the vault.
describeVaultOutput_sizeInBytes :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Integer)
describeVaultOutput_sizeInBytes = Lens.lens (\DescribeVaultOutput' {sizeInBytes} -> sizeInBytes) (\s@DescribeVaultOutput' {} a -> s {sizeInBytes = a} :: DescribeVaultOutput)

-- | The number of archives in the vault as of the last inventory date. This
-- field will return @null@ if an inventory has not yet run on the vault,
-- for example if you just created the vault.
describeVaultOutput_numberOfArchives :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Integer)
describeVaultOutput_numberOfArchives = Lens.lens (\DescribeVaultOutput' {numberOfArchives} -> numberOfArchives) (\s@DescribeVaultOutput' {} a -> s {numberOfArchives = a} :: DescribeVaultOutput)

instance Core.FromJSON DescribeVaultOutput where
  parseJSON =
    Core.withObject
      "DescribeVaultOutput"
      ( \x ->
          DescribeVaultOutput'
            Core.<$> (x Core..:? "LastInventoryDate")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "VaultName")
            Core.<*> (x Core..:? "VaultARN")
            Core.<*> (x Core..:? "SizeInBytes")
            Core.<*> (x Core..:? "NumberOfArchives")
      )

instance Core.Hashable DescribeVaultOutput

instance Core.NFData DescribeVaultOutput
