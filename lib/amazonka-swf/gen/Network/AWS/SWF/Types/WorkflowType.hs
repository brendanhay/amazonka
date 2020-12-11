-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowType
  ( WorkflowType (..),

    -- * Smart constructor
    mkWorkflowType,

    -- * Lenses
    wtName,
    wtVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a workflow type.
--
-- /See:/ 'mkWorkflowType' smart constructor.
data WorkflowType = WorkflowType'
  { name :: Lude.Text,
    version :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowType' with the minimum fields required to make a request.
--
-- * 'name' - The name of the workflow type.
-- * 'version' - The version of the workflow type.
mkWorkflowType ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  WorkflowType
mkWorkflowType pName_ pVersion_ =
  WorkflowType' {name = pName_, version = pVersion_}

-- | The name of the workflow type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtName :: Lens.Lens' WorkflowType Lude.Text
wtName = Lens.lens (name :: WorkflowType -> Lude.Text) (\s a -> s {name = a} :: WorkflowType)
{-# DEPRECATED wtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the workflow type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtVersion :: Lens.Lens' WorkflowType Lude.Text
wtVersion = Lens.lens (version :: WorkflowType -> Lude.Text) (\s a -> s {version = a} :: WorkflowType)
{-# DEPRECATED wtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON WorkflowType where
  parseJSON =
    Lude.withObject
      "WorkflowType"
      ( \x ->
          WorkflowType'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "version")
      )

instance Lude.ToJSON WorkflowType where
  toJSON WorkflowType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("version" Lude..= version)
          ]
      )
