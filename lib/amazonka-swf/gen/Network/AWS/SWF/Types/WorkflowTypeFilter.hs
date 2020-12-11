-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeFilter
  ( WorkflowTypeFilter (..),

    -- * Smart constructor
    mkWorkflowTypeFilter,

    -- * Lenses
    wtfVersion,
    wtfName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to filter workflow execution query results by type. Each parameter, if specified, defines a rule that must be satisfied by each returned result.
--
-- /See:/ 'mkWorkflowTypeFilter' smart constructor.
data WorkflowTypeFilter = WorkflowTypeFilter'
  { version ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowTypeFilter' with the minimum fields required to make a request.
--
-- * 'name' - Name of the workflow type.
-- * 'version' - Version of the workflow type.
mkWorkflowTypeFilter ::
  -- | 'name'
  Lude.Text ->
  WorkflowTypeFilter
mkWorkflowTypeFilter pName_ =
  WorkflowTypeFilter' {version = Lude.Nothing, name = pName_}

-- | Version of the workflow type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtfVersion :: Lens.Lens' WorkflowTypeFilter (Lude.Maybe Lude.Text)
wtfVersion = Lens.lens (version :: WorkflowTypeFilter -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: WorkflowTypeFilter)
{-# DEPRECATED wtfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | Name of the workflow type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtfName :: Lens.Lens' WorkflowTypeFilter Lude.Text
wtfName = Lens.lens (name :: WorkflowTypeFilter -> Lude.Text) (\s a -> s {name = a} :: WorkflowTypeFilter)
{-# DEPRECATED wtfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON WorkflowTypeFilter where
  toJSON WorkflowTypeFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("version" Lude..=) Lude.<$> version,
            Lude.Just ("name" Lude..= name)
          ]
      )
