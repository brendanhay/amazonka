-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CatalogImportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CatalogImportStatus
  ( CatalogImportStatus (..),

    -- * Smart constructor
    mkCatalogImportStatus,

    -- * Lenses
    cisImportedBy,
    cisImportTime,
    cisImportCompleted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure containing migration status information.
--
-- /See:/ 'mkCatalogImportStatus' smart constructor.
data CatalogImportStatus = CatalogImportStatus'
  { importedBy ::
      Lude.Maybe Lude.Text,
    importTime :: Lude.Maybe Lude.Timestamp,
    importCompleted :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CatalogImportStatus' with the minimum fields required to make a request.
--
-- * 'importCompleted' - @True@ if the migration has completed, or @False@ otherwise.
-- * 'importTime' - The time that the migration was started.
-- * 'importedBy' - The name of the person who initiated the migration.
mkCatalogImportStatus ::
  CatalogImportStatus
mkCatalogImportStatus =
  CatalogImportStatus'
    { importedBy = Lude.Nothing,
      importTime = Lude.Nothing,
      importCompleted = Lude.Nothing
    }

-- | The name of the person who initiated the migration.
--
-- /Note:/ Consider using 'importedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisImportedBy :: Lens.Lens' CatalogImportStatus (Lude.Maybe Lude.Text)
cisImportedBy = Lens.lens (importedBy :: CatalogImportStatus -> Lude.Maybe Lude.Text) (\s a -> s {importedBy = a} :: CatalogImportStatus)
{-# DEPRECATED cisImportedBy "Use generic-lens or generic-optics with 'importedBy' instead." #-}

-- | The time that the migration was started.
--
-- /Note:/ Consider using 'importTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisImportTime :: Lens.Lens' CatalogImportStatus (Lude.Maybe Lude.Timestamp)
cisImportTime = Lens.lens (importTime :: CatalogImportStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {importTime = a} :: CatalogImportStatus)
{-# DEPRECATED cisImportTime "Use generic-lens or generic-optics with 'importTime' instead." #-}

-- | @True@ if the migration has completed, or @False@ otherwise.
--
-- /Note:/ Consider using 'importCompleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisImportCompleted :: Lens.Lens' CatalogImportStatus (Lude.Maybe Lude.Bool)
cisImportCompleted = Lens.lens (importCompleted :: CatalogImportStatus -> Lude.Maybe Lude.Bool) (\s a -> s {importCompleted = a} :: CatalogImportStatus)
{-# DEPRECATED cisImportCompleted "Use generic-lens or generic-optics with 'importCompleted' instead." #-}

instance Lude.FromJSON CatalogImportStatus where
  parseJSON =
    Lude.withObject
      "CatalogImportStatus"
      ( \x ->
          CatalogImportStatus'
            Lude.<$> (x Lude..:? "ImportedBy")
            Lude.<*> (x Lude..:? "ImportTime")
            Lude.<*> (x Lude..:? "ImportCompleted")
      )
