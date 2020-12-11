-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ExportFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ExportFilter
  ( ExportFilter (..),

    -- * Smart constructor
    mkExportFilter,

    -- * Lenses
    efName,
    efValues,
    efCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html StartExportTask> action.
--
-- /See:/ 'mkExportFilter' smart constructor.
data ExportFilter = ExportFilter'
  { name :: Lude.Text,
    values :: [Lude.Text],
    condition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportFilter' with the minimum fields required to make a request.
--
-- * 'condition' - Supported condition: @EQUALS@
-- * 'name' - A single @ExportFilter@ name. Supported filters: @agentId@ .
-- * 'values' - A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
mkExportFilter ::
  -- | 'name'
  Lude.Text ->
  -- | 'condition'
  Lude.Text ->
  ExportFilter
mkExportFilter pName_ pCondition_ =
  ExportFilter'
    { name = pName_,
      values = Lude.mempty,
      condition = pCondition_
    }

-- | A single @ExportFilter@ name. Supported filters: @agentId@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efName :: Lens.Lens' ExportFilter Lude.Text
efName = Lens.lens (name :: ExportFilter -> Lude.Text) (\s a -> s {name = a} :: ExportFilter)
{-# DEPRECATED efName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A single @agentId@ for a Discovery Agent. An @agentId@ can be found using the <http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_DescribeExportTasks.html DescribeAgents> action. Typically an ADS @agentId@ is in the form @o-0123456789abcdef0@ .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValues :: Lens.Lens' ExportFilter [Lude.Text]
efValues = Lens.lens (values :: ExportFilter -> [Lude.Text]) (\s a -> s {values = a} :: ExportFilter)
{-# DEPRECATED efValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Supported condition: @EQUALS@
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efCondition :: Lens.Lens' ExportFilter Lude.Text
efCondition = Lens.lens (condition :: ExportFilter -> Lude.Text) (\s a -> s {condition = a} :: ExportFilter)
{-# DEPRECATED efCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.ToJSON ExportFilter where
  toJSON ExportFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("values" Lude..= values),
            Lude.Just ("condition" Lude..= condition)
          ]
      )
