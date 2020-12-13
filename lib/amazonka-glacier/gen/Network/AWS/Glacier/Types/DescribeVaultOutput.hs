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
    dvoVaultName,
    dvoSizeInBytes,
    dvoLastInventoryDate,
    dvoVaultARN,
    dvoCreationDate,
    dvoNumberOfArchives,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkDescribeVaultOutput' smart constructor.
data DescribeVaultOutput = DescribeVaultOutput'
  { -- | The name of the vault.
    vaultName :: Lude.Maybe Lude.Text,
    -- | Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
    sizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
    lastInventoryDate :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the vault.
    vaultARN :: Lude.Maybe Lude.Text,
    -- | The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
    creationDate :: Lude.Maybe Lude.Text,
    -- | The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
    numberOfArchives :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVaultOutput' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'sizeInBytes' - Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
-- * 'lastInventoryDate' - The Universal Coordinated Time (UTC) date when Amazon S3 Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
-- * 'vaultARN' - The Amazon Resource Name (ARN) of the vault.
-- * 'creationDate' - The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
-- * 'numberOfArchives' - The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
mkDescribeVaultOutput ::
  DescribeVaultOutput
mkDescribeVaultOutput =
  DescribeVaultOutput'
    { vaultName = Lude.Nothing,
      sizeInBytes = Lude.Nothing,
      lastInventoryDate = Lude.Nothing,
      vaultARN = Lude.Nothing,
      creationDate = Lude.Nothing,
      numberOfArchives = Lude.Nothing
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoVaultName :: Lens.Lens' DescribeVaultOutput (Lude.Maybe Lude.Text)
dvoVaultName = Lens.lens (vaultName :: DescribeVaultOutput -> Lude.Maybe Lude.Text) (\s a -> s {vaultName = a} :: DescribeVaultOutput)
{-# DEPRECATED dvoVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | Total size, in bytes, of the archives in the vault as of the last inventory date. This field will return null if an inventory has not yet run on the vault, for example if you just created the vault.
--
-- /Note:/ Consider using 'sizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoSizeInBytes :: Lens.Lens' DescribeVaultOutput (Lude.Maybe Lude.Integer)
dvoSizeInBytes = Lens.lens (sizeInBytes :: DescribeVaultOutput -> Lude.Maybe Lude.Integer) (\s a -> s {sizeInBytes = a} :: DescribeVaultOutput)
{-# DEPRECATED dvoSizeInBytes "Use generic-lens or generic-optics with 'sizeInBytes' instead." #-}

-- | The Universal Coordinated Time (UTC) date when Amazon S3 Glacier completed the last vault inventory. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
--
-- /Note:/ Consider using 'lastInventoryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoLastInventoryDate :: Lens.Lens' DescribeVaultOutput (Lude.Maybe Lude.Text)
dvoLastInventoryDate = Lens.lens (lastInventoryDate :: DescribeVaultOutput -> Lude.Maybe Lude.Text) (\s a -> s {lastInventoryDate = a} :: DescribeVaultOutput)
{-# DEPRECATED dvoLastInventoryDate "Use generic-lens or generic-optics with 'lastInventoryDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoVaultARN :: Lens.Lens' DescribeVaultOutput (Lude.Maybe Lude.Text)
dvoVaultARN = Lens.lens (vaultARN :: DescribeVaultOutput -> Lude.Maybe Lude.Text) (\s a -> s {vaultARN = a} :: DescribeVaultOutput)
{-# DEPRECATED dvoVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

-- | The Universal Coordinated Time (UTC) date when the vault was created. This value should be a string in the ISO 8601 date format, for example @2012-03-20T17:03:43.221Z@ .
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoCreationDate :: Lens.Lens' DescribeVaultOutput (Lude.Maybe Lude.Text)
dvoCreationDate = Lens.lens (creationDate :: DescribeVaultOutput -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: DescribeVaultOutput)
{-# DEPRECATED dvoCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The number of archives in the vault as of the last inventory date. This field will return @null@ if an inventory has not yet run on the vault, for example if you just created the vault.
--
-- /Note:/ Consider using 'numberOfArchives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoNumberOfArchives :: Lens.Lens' DescribeVaultOutput (Lude.Maybe Lude.Integer)
dvoNumberOfArchives = Lens.lens (numberOfArchives :: DescribeVaultOutput -> Lude.Maybe Lude.Integer) (\s a -> s {numberOfArchives = a} :: DescribeVaultOutput)
{-# DEPRECATED dvoNumberOfArchives "Use generic-lens or generic-optics with 'numberOfArchives' instead." #-}

instance Lude.FromJSON DescribeVaultOutput where
  parseJSON =
    Lude.withObject
      "DescribeVaultOutput"
      ( \x ->
          DescribeVaultOutput'
            Lude.<$> (x Lude..:? "VaultName")
            Lude.<*> (x Lude..:? "SizeInBytes")
            Lude.<*> (x Lude..:? "LastInventoryDate")
            Lude.<*> (x Lude..:? "VaultARN")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "NumberOfArchives")
      )
