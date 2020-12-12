{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilter
  ( ImportTaskFilter (..),

    -- * Smart constructor
    mkImportTaskFilter,

    -- * Lenses
    itfValues,
    itfName,
  )
where

import Network.AWS.Discovery.Types.ImportTaskFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A name-values pair of elements you can use to filter the results when querying your import tasks. Currently, wildcards are not supported for filters.
--
-- /See:/ 'mkImportTaskFilter' smart constructor.
data ImportTaskFilter = ImportTaskFilter'
  { values ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    name :: Lude.Maybe ImportTaskFilterName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportTaskFilter' with the minimum fields required to make a request.
--
-- * 'name' - The name, status, or import task ID for a specific import task.
-- * 'values' - An array of strings that you can provide to match against a specific name, status, or import task ID to filter the results for your import task queries.
mkImportTaskFilter ::
  ImportTaskFilter
mkImportTaskFilter =
  ImportTaskFilter' {values = Lude.Nothing, name = Lude.Nothing}

-- | An array of strings that you can provide to match against a specific name, status, or import task ID to filter the results for your import task queries.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfValues :: Lens.Lens' ImportTaskFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
itfValues = Lens.lens (values :: ImportTaskFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {values = a} :: ImportTaskFilter)
{-# DEPRECATED itfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name, status, or import task ID for a specific import task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfName :: Lens.Lens' ImportTaskFilter (Lude.Maybe ImportTaskFilterName)
itfName = Lens.lens (name :: ImportTaskFilter -> Lude.Maybe ImportTaskFilterName) (\s a -> s {name = a} :: ImportTaskFilter)
{-# DEPRECATED itfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON ImportTaskFilter where
  toJSON ImportTaskFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("values" Lude..=) Lude.<$> values,
            ("name" Lude..=) Lude.<$> name
          ]
      )
