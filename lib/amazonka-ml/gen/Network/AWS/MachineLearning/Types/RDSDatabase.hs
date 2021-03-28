{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.RDSDatabase
  ( RDSDatabase (..)
  -- * Smart constructor
  , mkRDSDatabase
  -- * Lenses
  , rdsdInstanceIdentifier
  , rdsdDatabaseName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.RDSDatabaseName as Types
import qualified Network.AWS.MachineLearning.Types.RDSInstanceIdentifier as Types
import qualified Network.AWS.Prelude as Core

-- | The database details of an Amazon RDS database.
--
-- /See:/ 'mkRDSDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
  { instanceIdentifier :: Types.RDSInstanceIdentifier
    -- ^ The ID of an RDS DB instance.
  , databaseName :: Types.RDSDatabaseName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RDSDatabase' value with any optional fields omitted.
mkRDSDatabase
    :: Types.RDSInstanceIdentifier -- ^ 'instanceIdentifier'
    -> Types.RDSDatabaseName -- ^ 'databaseName'
    -> RDSDatabase
mkRDSDatabase instanceIdentifier databaseName
  = RDSDatabase'{instanceIdentifier, databaseName}

-- | The ID of an RDS DB instance.
--
-- /Note:/ Consider using 'instanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdInstanceIdentifier :: Lens.Lens' RDSDatabase Types.RDSInstanceIdentifier
rdsdInstanceIdentifier = Lens.field @"instanceIdentifier"
{-# INLINEABLE rdsdInstanceIdentifier #-}
{-# DEPRECATED instanceIdentifier "Use generic-lens or generic-optics with 'instanceIdentifier' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdDatabaseName :: Lens.Lens' RDSDatabase Types.RDSDatabaseName
rdsdDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE rdsdDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

instance Core.FromJSON RDSDatabase where
        toJSON RDSDatabase{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceIdentifier" Core..= instanceIdentifier),
                  Core.Just ("DatabaseName" Core..= databaseName)])

instance Core.FromJSON RDSDatabase where
        parseJSON
          = Core.withObject "RDSDatabase" Core.$
              \ x ->
                RDSDatabase' Core.<$>
                  (x Core..: "InstanceIdentifier") Core.<*> x Core..: "DatabaseName"
