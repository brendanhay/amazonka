{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterDBRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterDBRevision
  ( ClusterDBRevision (..),

    -- * Smart constructor
    mkClusterDBRevision,

    -- * Lenses
    cdrDatabaseRevisionReleaseDate,
    cdrClusterIdentifier,
    cdrCurrentDatabaseRevision,
    cdrRevisionTargets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RevisionTarget

-- | Describes a @ClusterDbRevision@ .
--
-- /See:/ 'mkClusterDBRevision' smart constructor.
data ClusterDBRevision = ClusterDBRevision'
  { -- | The date on which the database revision was released.
    databaseRevisionReleaseDate :: Lude.Maybe Lude.DateTime,
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | A string representing the current cluster version.
    currentDatabaseRevision :: Lude.Maybe Lude.Text,
    -- | A list of @RevisionTarget@ objects, where each object describes the database revision that a cluster can be updated to.
    revisionTargets :: Lude.Maybe [RevisionTarget]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterDBRevision' with the minimum fields required to make a request.
--
-- * 'databaseRevisionReleaseDate' - The date on which the database revision was released.
-- * 'clusterIdentifier' - The unique identifier of the cluster.
-- * 'currentDatabaseRevision' - A string representing the current cluster version.
-- * 'revisionTargets' - A list of @RevisionTarget@ objects, where each object describes the database revision that a cluster can be updated to.
mkClusterDBRevision ::
  ClusterDBRevision
mkClusterDBRevision =
  ClusterDBRevision'
    { databaseRevisionReleaseDate = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      currentDatabaseRevision = Lude.Nothing,
      revisionTargets = Lude.Nothing
    }

-- | The date on which the database revision was released.
--
-- /Note:/ Consider using 'databaseRevisionReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrDatabaseRevisionReleaseDate :: Lens.Lens' ClusterDBRevision (Lude.Maybe Lude.DateTime)
cdrDatabaseRevisionReleaseDate = Lens.lens (databaseRevisionReleaseDate :: ClusterDBRevision -> Lude.Maybe Lude.DateTime) (\s a -> s {databaseRevisionReleaseDate = a} :: ClusterDBRevision)
{-# DEPRECATED cdrDatabaseRevisionReleaseDate "Use generic-lens or generic-optics with 'databaseRevisionReleaseDate' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrClusterIdentifier :: Lens.Lens' ClusterDBRevision (Lude.Maybe Lude.Text)
cdrClusterIdentifier = Lens.lens (clusterIdentifier :: ClusterDBRevision -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: ClusterDBRevision)
{-# DEPRECATED cdrClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A string representing the current cluster version.
--
-- /Note:/ Consider using 'currentDatabaseRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrCurrentDatabaseRevision :: Lens.Lens' ClusterDBRevision (Lude.Maybe Lude.Text)
cdrCurrentDatabaseRevision = Lens.lens (currentDatabaseRevision :: ClusterDBRevision -> Lude.Maybe Lude.Text) (\s a -> s {currentDatabaseRevision = a} :: ClusterDBRevision)
{-# DEPRECATED cdrCurrentDatabaseRevision "Use generic-lens or generic-optics with 'currentDatabaseRevision' instead." #-}

-- | A list of @RevisionTarget@ objects, where each object describes the database revision that a cluster can be updated to.
--
-- /Note:/ Consider using 'revisionTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrRevisionTargets :: Lens.Lens' ClusterDBRevision (Lude.Maybe [RevisionTarget])
cdrRevisionTargets = Lens.lens (revisionTargets :: ClusterDBRevision -> Lude.Maybe [RevisionTarget]) (\s a -> s {revisionTargets = a} :: ClusterDBRevision)
{-# DEPRECATED cdrRevisionTargets "Use generic-lens or generic-optics with 'revisionTargets' instead." #-}

instance Lude.FromXML ClusterDBRevision where
  parseXML x =
    ClusterDBRevision'
      Lude.<$> (x Lude..@? "DatabaseRevisionReleaseDate")
      Lude.<*> (x Lude..@? "ClusterIdentifier")
      Lude.<*> (x Lude..@? "CurrentDatabaseRevision")
      Lude.<*> ( x Lude..@? "RevisionTargets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "RevisionTarget")
               )
