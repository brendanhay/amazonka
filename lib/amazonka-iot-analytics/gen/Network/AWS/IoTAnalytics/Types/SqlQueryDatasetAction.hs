-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SqlQueryDatasetAction
  ( SqlQueryDatasetAction (..),

    -- * Smart constructor
    mkSqlQueryDatasetAction,

    -- * Lenses
    sqdaFilters,
    sqdaSqlQuery,
  )
where

import Network.AWS.IoTAnalytics.Types.QueryFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The SQL query to modify the message.
--
-- /See:/ 'mkSqlQueryDatasetAction' smart constructor.
data SqlQueryDatasetAction = SqlQueryDatasetAction'
  { filters ::
      Lude.Maybe [QueryFilter],
    sqlQuery :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqlQueryDatasetAction' with the minimum fields required to make a request.
--
-- * 'filters' - Prefilters applied to message data.
-- * 'sqlQuery' - A SQL query string.
mkSqlQueryDatasetAction ::
  -- | 'sqlQuery'
  Lude.Text ->
  SqlQueryDatasetAction
mkSqlQueryDatasetAction pSqlQuery_ =
  SqlQueryDatasetAction'
    { filters = Lude.Nothing,
      sqlQuery = pSqlQuery_
    }

-- | Prefilters applied to message data.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqdaFilters :: Lens.Lens' SqlQueryDatasetAction (Lude.Maybe [QueryFilter])
sqdaFilters = Lens.lens (filters :: SqlQueryDatasetAction -> Lude.Maybe [QueryFilter]) (\s a -> s {filters = a} :: SqlQueryDatasetAction)
{-# DEPRECATED sqdaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A SQL query string.
--
-- /Note:/ Consider using 'sqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqdaSqlQuery :: Lens.Lens' SqlQueryDatasetAction Lude.Text
sqdaSqlQuery = Lens.lens (sqlQuery :: SqlQueryDatasetAction -> Lude.Text) (\s a -> s {sqlQuery = a} :: SqlQueryDatasetAction)
{-# DEPRECATED sqdaSqlQuery "Use generic-lens or generic-optics with 'sqlQuery' instead." #-}

instance Lude.FromJSON SqlQueryDatasetAction where
  parseJSON =
    Lude.withObject
      "SqlQueryDatasetAction"
      ( \x ->
          SqlQueryDatasetAction'
            Lude.<$> (x Lude..:? "filters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "sqlQuery")
      )

instance Lude.ToJSON SqlQueryDatasetAction where
  toJSON SqlQueryDatasetAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filters" Lude..=) Lude.<$> filters,
            Lude.Just ("sqlQuery" Lude..= sqlQuery)
          ]
      )
