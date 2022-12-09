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
-- Module      : Amazonka.Glacier.Types.DescribeVaultOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.DescribeVaultOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newDescribeVaultOutput' smart constructor.
data DescribeVaultOutput = DescribeVaultOutput'
  { -- | The Universal Coordinated Time (UTC) date when the vault was created.
    -- This value should be a string in the ISO 8601 date format, for example
    -- @2012-03-20T17:03:43.221Z@.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier
    -- completed the last vault inventory. This value should be a string in the
    -- ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@.
    lastInventoryDate :: Prelude.Maybe Prelude.Text,
    -- | The number of archives in the vault as of the last inventory date. This
    -- field will return @null@ if an inventory has not yet run on the vault,
    -- for example if you just created the vault.
    numberOfArchives :: Prelude.Maybe Prelude.Integer,
    -- | Total size, in bytes, of the archives in the vault as of the last
    -- inventory date. This field will return null if an inventory has not yet
    -- run on the vault, for example if you just created the vault.
    sizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the vault.
    vaultARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVaultOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'describeVaultOutput_creationDate' - The Universal Coordinated Time (UTC) date when the vault was created.
-- This value should be a string in the ISO 8601 date format, for example
-- @2012-03-20T17:03:43.221Z@.
--
-- 'lastInventoryDate', 'describeVaultOutput_lastInventoryDate' - The Universal Coordinated Time (UTC) date when Amazon S3 Glacier
-- completed the last vault inventory. This value should be a string in the
-- ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@.
--
-- 'numberOfArchives', 'describeVaultOutput_numberOfArchives' - The number of archives in the vault as of the last inventory date. This
-- field will return @null@ if an inventory has not yet run on the vault,
-- for example if you just created the vault.
--
-- 'sizeInBytes', 'describeVaultOutput_sizeInBytes' - Total size, in bytes, of the archives in the vault as of the last
-- inventory date. This field will return null if an inventory has not yet
-- run on the vault, for example if you just created the vault.
--
-- 'vaultARN', 'describeVaultOutput_vaultARN' - The Amazon Resource Name (ARN) of the vault.
--
-- 'vaultName', 'describeVaultOutput_vaultName' - The name of the vault.
newDescribeVaultOutput ::
  DescribeVaultOutput
newDescribeVaultOutput =
  DescribeVaultOutput'
    { creationDate =
        Prelude.Nothing,
      lastInventoryDate = Prelude.Nothing,
      numberOfArchives = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      vaultARN = Prelude.Nothing,
      vaultName = Prelude.Nothing
    }

-- | The Universal Coordinated Time (UTC) date when the vault was created.
-- This value should be a string in the ISO 8601 date format, for example
-- @2012-03-20T17:03:43.221Z@.
describeVaultOutput_creationDate :: Lens.Lens' DescribeVaultOutput (Prelude.Maybe Prelude.Text)
describeVaultOutput_creationDate = Lens.lens (\DescribeVaultOutput' {creationDate} -> creationDate) (\s@DescribeVaultOutput' {} a -> s {creationDate = a} :: DescribeVaultOutput)

-- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier
-- completed the last vault inventory. This value should be a string in the
-- ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@.
describeVaultOutput_lastInventoryDate :: Lens.Lens' DescribeVaultOutput (Prelude.Maybe Prelude.Text)
describeVaultOutput_lastInventoryDate = Lens.lens (\DescribeVaultOutput' {lastInventoryDate} -> lastInventoryDate) (\s@DescribeVaultOutput' {} a -> s {lastInventoryDate = a} :: DescribeVaultOutput)

-- | The number of archives in the vault as of the last inventory date. This
-- field will return @null@ if an inventory has not yet run on the vault,
-- for example if you just created the vault.
describeVaultOutput_numberOfArchives :: Lens.Lens' DescribeVaultOutput (Prelude.Maybe Prelude.Integer)
describeVaultOutput_numberOfArchives = Lens.lens (\DescribeVaultOutput' {numberOfArchives} -> numberOfArchives) (\s@DescribeVaultOutput' {} a -> s {numberOfArchives = a} :: DescribeVaultOutput)

-- | Total size, in bytes, of the archives in the vault as of the last
-- inventory date. This field will return null if an inventory has not yet
-- run on the vault, for example if you just created the vault.
describeVaultOutput_sizeInBytes :: Lens.Lens' DescribeVaultOutput (Prelude.Maybe Prelude.Integer)
describeVaultOutput_sizeInBytes = Lens.lens (\DescribeVaultOutput' {sizeInBytes} -> sizeInBytes) (\s@DescribeVaultOutput' {} a -> s {sizeInBytes = a} :: DescribeVaultOutput)

-- | The Amazon Resource Name (ARN) of the vault.
describeVaultOutput_vaultARN :: Lens.Lens' DescribeVaultOutput (Prelude.Maybe Prelude.Text)
describeVaultOutput_vaultARN = Lens.lens (\DescribeVaultOutput' {vaultARN} -> vaultARN) (\s@DescribeVaultOutput' {} a -> s {vaultARN = a} :: DescribeVaultOutput)

-- | The name of the vault.
describeVaultOutput_vaultName :: Lens.Lens' DescribeVaultOutput (Prelude.Maybe Prelude.Text)
describeVaultOutput_vaultName = Lens.lens (\DescribeVaultOutput' {vaultName} -> vaultName) (\s@DescribeVaultOutput' {} a -> s {vaultName = a} :: DescribeVaultOutput)

instance Data.FromJSON DescribeVaultOutput where
  parseJSON =
    Data.withObject
      "DescribeVaultOutput"
      ( \x ->
          DescribeVaultOutput'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "LastInventoryDate")
            Prelude.<*> (x Data..:? "NumberOfArchives")
            Prelude.<*> (x Data..:? "SizeInBytes")
            Prelude.<*> (x Data..:? "VaultARN")
            Prelude.<*> (x Data..:? "VaultName")
      )

instance Prelude.Hashable DescribeVaultOutput where
  hashWithSalt _salt DescribeVaultOutput' {..} =
    _salt `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastInventoryDate
      `Prelude.hashWithSalt` numberOfArchives
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` vaultARN
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData DescribeVaultOutput where
  rnf DescribeVaultOutput' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastInventoryDate
      `Prelude.seq` Prelude.rnf numberOfArchives
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf vaultARN
      `Prelude.seq` Prelude.rnf vaultName
