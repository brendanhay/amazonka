{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DataSource
  ( DataSource (..),

    -- * Smart constructor
    mkDataSource,

    -- * Lenses
    dsArn,
    dsDatabaseName,
    dsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an app's data source.
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The data source's ARN.
    arn :: Core.Maybe Types.String,
    -- | The database name.
    databaseName :: Core.Maybe Types.String,
    -- | The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataSource' value with any optional fields omitted.
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { arn = Core.Nothing,
      databaseName = Core.Nothing,
      type' = Core.Nothing
    }

-- | The data source's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsArn :: Lens.Lens' DataSource (Core.Maybe Types.String)
dsArn = Lens.field @"arn"
{-# DEPRECATED dsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The database name.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDatabaseName :: Lens.Lens' DataSource (Core.Maybe Types.String)
dsDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsType :: Lens.Lens' DataSource (Core.Maybe Types.String)
dsType = Lens.field @"type'"
{-# DEPRECATED dsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON DataSource where
  toJSON DataSource {..} =
    Core.object
      ( Core.catMaybes
          [ ("Arn" Core..=) Core.<$> arn,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON DataSource where
  parseJSON =
    Core.withObject "DataSource" Core.$
      \x ->
        DataSource'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "DatabaseName")
          Core.<*> (x Core..:? "Type")
