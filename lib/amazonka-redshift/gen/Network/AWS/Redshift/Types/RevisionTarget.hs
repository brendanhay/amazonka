{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RevisionTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RevisionTarget
  ( RevisionTarget (..),

    -- * Smart constructor
    mkRevisionTarget,

    -- * Lenses
    rtDatabaseRevisionReleaseDate,
    rtDatabaseRevision,
    rtDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a @RevisionTarget@ .
--
-- /See:/ 'mkRevisionTarget' smart constructor.
data RevisionTarget = RevisionTarget'
  { databaseRevisionReleaseDate ::
      Lude.Maybe Lude.DateTime,
    databaseRevision :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevisionTarget' with the minimum fields required to make a request.
--
-- * 'databaseRevision' - A unique string that identifies the version to update the cluster to. You can use this value in 'ModifyClusterDbRevision' .
-- * 'databaseRevisionReleaseDate' - The date on which the database revision was released.
-- * 'description' - A string that describes the changes and features that will be applied to the cluster when it is updated to the corresponding 'ClusterDbRevision' .
mkRevisionTarget ::
  RevisionTarget
mkRevisionTarget =
  RevisionTarget'
    { databaseRevisionReleaseDate = Lude.Nothing,
      databaseRevision = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date on which the database revision was released.
--
-- /Note:/ Consider using 'databaseRevisionReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDatabaseRevisionReleaseDate :: Lens.Lens' RevisionTarget (Lude.Maybe Lude.DateTime)
rtDatabaseRevisionReleaseDate = Lens.lens (databaseRevisionReleaseDate :: RevisionTarget -> Lude.Maybe Lude.DateTime) (\s a -> s {databaseRevisionReleaseDate = a} :: RevisionTarget)
{-# DEPRECATED rtDatabaseRevisionReleaseDate "Use generic-lens or generic-optics with 'databaseRevisionReleaseDate' instead." #-}

-- | A unique string that identifies the version to update the cluster to. You can use this value in 'ModifyClusterDbRevision' .
--
-- /Note:/ Consider using 'databaseRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDatabaseRevision :: Lens.Lens' RevisionTarget (Lude.Maybe Lude.Text)
rtDatabaseRevision = Lens.lens (databaseRevision :: RevisionTarget -> Lude.Maybe Lude.Text) (\s a -> s {databaseRevision = a} :: RevisionTarget)
{-# DEPRECATED rtDatabaseRevision "Use generic-lens or generic-optics with 'databaseRevision' instead." #-}

-- | A string that describes the changes and features that will be applied to the cluster when it is updated to the corresponding 'ClusterDbRevision' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDescription :: Lens.Lens' RevisionTarget (Lude.Maybe Lude.Text)
rtDescription = Lens.lens (description :: RevisionTarget -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RevisionTarget)
{-# DEPRECATED rtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML RevisionTarget where
  parseXML x =
    RevisionTarget'
      Lude.<$> (x Lude..@? "DatabaseRevisionReleaseDate")
      Lude.<*> (x Lude..@? "DatabaseRevision")
      Lude.<*> (x Lude..@? "Description")
