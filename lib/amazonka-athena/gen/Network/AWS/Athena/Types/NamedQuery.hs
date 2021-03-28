{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.NamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.NamedQuery
  ( NamedQuery (..)
  -- * Smart constructor
  , mkNamedQuery
  -- * Lenses
  , nqName
  , nqDatabase
  , nqQueryString
  , nqDescription
  , nqNamedQueryId
  , nqWorkGroup
  ) where

import qualified Network.AWS.Athena.Types.DatabaseString as Types
import qualified Network.AWS.Athena.Types.DescriptionString as Types
import qualified Network.AWS.Athena.Types.NameString as Types
import qualified Network.AWS.Athena.Types.NamedQueryId as Types
import qualified Network.AWS.Athena.Types.QueryString as Types
import qualified Network.AWS.Athena.Types.WorkGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A query, where @QueryString@ is the list of SQL query statements that comprise the query.
--
-- /See:/ 'mkNamedQuery' smart constructor.
data NamedQuery = NamedQuery'
  { name :: Types.NameString
    -- ^ The query name.
  , database :: Types.DatabaseString
    -- ^ The database to which the query belongs.
  , queryString :: Types.QueryString
    -- ^ The SQL query statements that comprise the query.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ The query description.
  , namedQueryId :: Core.Maybe Types.NamedQueryId
    -- ^ The unique identifier of the query.
  , workGroup :: Core.Maybe Types.WorkGroupName
    -- ^ The name of the workgroup that contains the named query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NamedQuery' value with any optional fields omitted.
mkNamedQuery
    :: Types.NameString -- ^ 'name'
    -> Types.DatabaseString -- ^ 'database'
    -> Types.QueryString -- ^ 'queryString'
    -> NamedQuery
mkNamedQuery name database queryString
  = NamedQuery'{name, database, queryString,
                description = Core.Nothing, namedQueryId = Core.Nothing,
                workGroup = Core.Nothing}

-- | The query name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqName :: Lens.Lens' NamedQuery Types.NameString
nqName = Lens.field @"name"
{-# INLINEABLE nqName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The database to which the query belongs.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqDatabase :: Lens.Lens' NamedQuery Types.DatabaseString
nqDatabase = Lens.field @"database"
{-# INLINEABLE nqDatabase #-}
{-# DEPRECATED database "Use generic-lens or generic-optics with 'database' instead"  #-}

-- | The SQL query statements that comprise the query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqQueryString :: Lens.Lens' NamedQuery Types.QueryString
nqQueryString = Lens.field @"queryString"
{-# INLINEABLE nqQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

-- | The query description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqDescription :: Lens.Lens' NamedQuery (Core.Maybe Types.DescriptionString)
nqDescription = Lens.field @"description"
{-# INLINEABLE nqDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The unique identifier of the query.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqNamedQueryId :: Lens.Lens' NamedQuery (Core.Maybe Types.NamedQueryId)
nqNamedQueryId = Lens.field @"namedQueryId"
{-# INLINEABLE nqNamedQueryId #-}
{-# DEPRECATED namedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead"  #-}

-- | The name of the workgroup that contains the named query.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqWorkGroup :: Lens.Lens' NamedQuery (Core.Maybe Types.WorkGroupName)
nqWorkGroup = Lens.field @"workGroup"
{-# INLINEABLE nqWorkGroup #-}
{-# DEPRECATED workGroup "Use generic-lens or generic-optics with 'workGroup' instead"  #-}

instance Core.FromJSON NamedQuery where
        parseJSON
          = Core.withObject "NamedQuery" Core.$
              \ x ->
                NamedQuery' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Database" Core.<*>
                    x Core..: "QueryString"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "NamedQueryId"
                    Core.<*> x Core..:? "WorkGroup"
