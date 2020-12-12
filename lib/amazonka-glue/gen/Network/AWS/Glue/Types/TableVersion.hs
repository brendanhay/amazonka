{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableVersion
  ( TableVersion (..),

    -- * Smart constructor
    mkTableVersion,

    -- * Lenses
    tvVersionId,
    tvTable,
  )
where

import Network.AWS.Glue.Types.Table
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a version of a table.
--
-- /See:/ 'mkTableVersion' smart constructor.
data TableVersion = TableVersion'
  { versionId ::
      Lude.Maybe Lude.Text,
    table :: Lude.Maybe Table
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableVersion' with the minimum fields required to make a request.
--
-- * 'table' - The table in question.
-- * 'versionId' - The ID value that identifies this table version. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
mkTableVersion ::
  TableVersion
mkTableVersion =
  TableVersion' {versionId = Lude.Nothing, table = Lude.Nothing}

-- | The ID value that identifies this table version. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvVersionId :: Lens.Lens' TableVersion (Lude.Maybe Lude.Text)
tvVersionId = Lens.lens (versionId :: TableVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: TableVersion)
{-# DEPRECATED tvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The table in question.
--
-- /Note:/ Consider using 'table' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvTable :: Lens.Lens' TableVersion (Lude.Maybe Table)
tvTable = Lens.lens (table :: TableVersion -> Lude.Maybe Table) (\s a -> s {table = a} :: TableVersion)
{-# DEPRECATED tvTable "Use generic-lens or generic-optics with 'table' instead." #-}

instance Lude.FromJSON TableVersion where
  parseJSON =
    Lude.withObject
      "TableVersion"
      ( \x ->
          TableVersion'
            Lude.<$> (x Lude..:? "VersionId") Lude.<*> (x Lude..:? "Table")
      )
