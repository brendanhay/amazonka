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
    dsARN,
    dsDatabaseName,
    dsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an app's data source.
--
-- /See:/ 'mkDataSource' smart constructor.
data DataSource = DataSource'
  { -- | The data source's ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | The database name.
    databaseName :: Lude.Maybe Lude.Text,
    -- | The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- * 'arn' - The data source's ARN.
-- * 'databaseName' - The database name.
-- * 'type'' - The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
mkDataSource ::
  DataSource
mkDataSource =
  DataSource'
    { arn = Lude.Nothing,
      databaseName = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The data source's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsARN :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsARN = Lens.lens (arn :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DataSource)
{-# DEPRECATED dsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The database name.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDatabaseName :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsDatabaseName = Lens.lens (databaseName :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: DataSource)
{-# DEPRECATED dsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsType :: Lens.Lens' DataSource (Lude.Maybe Lude.Text)
dsType = Lens.lens (type' :: DataSource -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: DataSource)
{-# DEPRECATED dsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DataSource where
  parseJSON =
    Lude.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON DataSource where
  toJSON DataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Arn" Lude..=) Lude.<$> arn,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
