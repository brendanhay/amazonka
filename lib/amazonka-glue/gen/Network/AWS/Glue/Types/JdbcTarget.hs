{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JdbcTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JdbcTarget
  ( JdbcTarget (..),

    -- * Smart constructor
    mkJdbcTarget,

    -- * Lenses
    jtConnectionName,
    jtExclusions,
    jtPath,
  )
where

import qualified Network.AWS.Glue.Types.ConnectionName as Types
import qualified Network.AWS.Glue.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a JDBC data store to crawl.
--
-- /See:/ 'mkJdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
  { -- | The name of the connection to use to connect to the JDBC target.
    connectionName :: Core.Maybe Types.ConnectionName,
    -- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
    exclusions :: Core.Maybe [Types.Path],
    -- | The path of the JDBC target.
    path :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JdbcTarget' value with any optional fields omitted.
mkJdbcTarget ::
  JdbcTarget
mkJdbcTarget =
  JdbcTarget'
    { connectionName = Core.Nothing,
      exclusions = Core.Nothing,
      path = Core.Nothing
    }

-- | The name of the connection to use to connect to the JDBC target.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtConnectionName :: Lens.Lens' JdbcTarget (Core.Maybe Types.ConnectionName)
jtConnectionName = Lens.field @"connectionName"
{-# DEPRECATED jtConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
--
-- /Note:/ Consider using 'exclusions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtExclusions :: Lens.Lens' JdbcTarget (Core.Maybe [Types.Path])
jtExclusions = Lens.field @"exclusions"
{-# DEPRECATED jtExclusions "Use generic-lens or generic-optics with 'exclusions' instead." #-}

-- | The path of the JDBC target.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtPath :: Lens.Lens' JdbcTarget (Core.Maybe Types.Path)
jtPath = Lens.field @"path"
{-# DEPRECATED jtPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.FromJSON JdbcTarget where
  toJSON JdbcTarget {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionName" Core..=) Core.<$> connectionName,
            ("Exclusions" Core..=) Core.<$> exclusions,
            ("Path" Core..=) Core.<$> path
          ]
      )

instance Core.FromJSON JdbcTarget where
  parseJSON =
    Core.withObject "JdbcTarget" Core.$
      \x ->
        JdbcTarget'
          Core.<$> (x Core..:? "ConnectionName")
          Core.<*> (x Core..:? "Exclusions")
          Core.<*> (x Core..:? "Path")
