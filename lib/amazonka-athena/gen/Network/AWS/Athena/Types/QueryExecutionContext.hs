{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionContext
  ( QueryExecutionContext (..),

    -- * Smart constructor
    mkQueryExecutionContext,

    -- * Lenses
    qecDatabase,
    qecCatalog,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The database and data catalog context in which the query execution occurs.
--
-- /See:/ 'mkQueryExecutionContext' smart constructor.
data QueryExecutionContext = QueryExecutionContext'
  { -- | The name of the database used in the query execution.
    database :: Lude.Maybe Lude.Text,
    -- | The name of the data catalog used in the query execution.
    catalog :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryExecutionContext' with the minimum fields required to make a request.
--
-- * 'database' - The name of the database used in the query execution.
-- * 'catalog' - The name of the data catalog used in the query execution.
mkQueryExecutionContext ::
  QueryExecutionContext
mkQueryExecutionContext =
  QueryExecutionContext'
    { database = Lude.Nothing,
      catalog = Lude.Nothing
    }

-- | The name of the database used in the query execution.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qecDatabase :: Lens.Lens' QueryExecutionContext (Lude.Maybe Lude.Text)
qecDatabase = Lens.lens (database :: QueryExecutionContext -> Lude.Maybe Lude.Text) (\s a -> s {database = a} :: QueryExecutionContext)
{-# DEPRECATED qecDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | The name of the data catalog used in the query execution.
--
-- /Note:/ Consider using 'catalog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qecCatalog :: Lens.Lens' QueryExecutionContext (Lude.Maybe Lude.Text)
qecCatalog = Lens.lens (catalog :: QueryExecutionContext -> Lude.Maybe Lude.Text) (\s a -> s {catalog = a} :: QueryExecutionContext)
{-# DEPRECATED qecCatalog "Use generic-lens or generic-optics with 'catalog' instead." #-}

instance Lude.FromJSON QueryExecutionContext where
  parseJSON =
    Lude.withObject
      "QueryExecutionContext"
      ( \x ->
          QueryExecutionContext'
            Lude.<$> (x Lude..:? "Database") Lude.<*> (x Lude..:? "Catalog")
      )

instance Lude.ToJSON QueryExecutionContext where
  toJSON QueryExecutionContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Database" Lude..=) Lude.<$> database,
            ("Catalog" Lude..=) Lude.<$> catalog
          ]
      )
