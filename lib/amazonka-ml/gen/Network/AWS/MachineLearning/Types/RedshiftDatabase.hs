{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RedshiftDatabase
  ( RedshiftDatabase (..)
  -- * Smart constructor
  , mkRedshiftDatabase
  -- * Lenses
  , rdDatabaseName
  , rdClusterIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.DatabaseName as Types
import qualified Network.AWS.MachineLearning.Types.RedshiftClusterIdentifier as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the database details required to connect to an Amazon Redshift database.
--
-- /See:/ 'mkRedshiftDatabase' smart constructor.
data RedshiftDatabase = RedshiftDatabase'
  { databaseName :: Types.DatabaseName
  , clusterIdentifier :: Types.RedshiftClusterIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftDatabase' value with any optional fields omitted.
mkRedshiftDatabase
    :: Types.DatabaseName -- ^ 'databaseName'
    -> Types.RedshiftClusterIdentifier -- ^ 'clusterIdentifier'
    -> RedshiftDatabase
mkRedshiftDatabase databaseName clusterIdentifier
  = RedshiftDatabase'{databaseName, clusterIdentifier}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDatabaseName :: Lens.Lens' RedshiftDatabase Types.DatabaseName
rdDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE rdDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdClusterIdentifier :: Lens.Lens' RedshiftDatabase Types.RedshiftClusterIdentifier
rdClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rdClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.FromJSON RedshiftDatabase where
        toJSON RedshiftDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DatabaseName" Core..= databaseName),
                  Core.Just ("ClusterIdentifier" Core..= clusterIdentifier)])

instance Core.FromJSON RedshiftDatabase where
        parseJSON
          = Core.withObject "RedshiftDatabase" Core.$
              \ x ->
                RedshiftDatabase' Core.<$>
                  (x Core..: "DatabaseName") Core.<*> x Core..: "ClusterIdentifier"
