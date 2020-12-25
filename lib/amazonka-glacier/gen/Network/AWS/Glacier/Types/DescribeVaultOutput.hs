{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DescribeVaultOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DescribeVaultOutput
  ( DescribeVaultOutput (..),

    -- * Smart constructor
    mkDescribeVaultOutput,

    -- * Lenses
    dvoCreationDate,
    dvoLastInventoryDate,
    dvoNumberOfArchives,
    dvoSizeInBytes,
    dvoVaultARN,
    dvoVaultName,
  )
where

import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkDescribeVaultOutput' smart constructor.
data DescribeVaultOutput = DescribeVaultOutput'
  { -- | The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
    creationDate :: Core.Maybe Types.String,
    -- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
    lastInventoryDate :: Core.Maybe Types.String,
    -- | The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
    numberOfArchives :: Core.Maybe Core.Integer,
    -- | Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
    sizeInBytes :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the vault.
    vaultARN :: Core.Maybe Types.String,
    -- | The name of the vault.
    vaultName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVaultOutput' value with any optional fields omitted.
mkDescribeVaultOutput ::
  DescribeVaultOutput
mkDescribeVaultOutput =
  DescribeVaultOutput'
    { creationDate = Core.Nothing,
      lastInventoryDate = Core.Nothing,
      numberOfArchives = Core.Nothing,
      sizeInBytes = Core.Nothing,
      vaultARN = Core.Nothing,
      vaultName = Core.Nothing
    }

-- | The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoCreationDate :: Lens.Lens' DescribeVaultOutput (Core.Maybe Types.String)
dvoCreationDate = Lens.field @"creationDate"
{-# DEPRECATED dvoCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
--
-- /Note:/ Consider using 'lastInventoryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoLastInventoryDate :: Lens.Lens' DescribeVaultOutput (Core.Maybe Types.String)
dvoLastInventoryDate = Lens.field @"lastInventoryDate"
{-# DEPRECATED dvoLastInventoryDate "Use generic-lens or generic-optics with 'lastInventoryDate' instead." #-}

-- | The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
--
-- /Note:/ Consider using 'numberOfArchives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoNumberOfArchives :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Integer)
dvoNumberOfArchives = Lens.field @"numberOfArchives"
{-# DEPRECATED dvoNumberOfArchives "Use generic-lens or generic-optics with 'numberOfArchives' instead." #-}

-- | Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoSizeInBytes :: Lens.Lens' DescribeVaultOutput (Core.Maybe Core.Integer)
dvoSizeInBytes = Lens.field @"sizeInBytes"
{-# DEPRECATED dvoSizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoVaultARN :: Lens.Lens' DescribeVaultOutput (Core.Maybe Types.String)
dvoVaultARN = Lens.field @"vaultARN"
{-# DEPRECATED dvoVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoVaultName :: Lens.Lens' DescribeVaultOutput (Core.Maybe Types.String)
dvoVaultName = Lens.field @"vaultName"
{-# DEPRECATED dvoVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.FromJSON DescribeVaultOutput where
  parseJSON =
    Core.withObject "DescribeVaultOutput" Core.$
      \x ->
        DescribeVaultOutput'
          Core.<$> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "LastInventoryDate")
          Core.<*> (x Core..:? "NumberOfArchives")
          Core.<*> (x Core..:? "SizeInBytes")
          Core.<*> (x Core..:? "VaultARN")
          Core.<*> (x Core..:? "VaultName")
