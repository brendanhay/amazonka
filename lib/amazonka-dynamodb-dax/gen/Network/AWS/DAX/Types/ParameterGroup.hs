{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroup
  ( ParameterGroup (..),

    -- * Smart constructor
    mkParameterGroup,

    -- * Lenses
    pgDescription,
    pgParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A named set of parameters that are applied to all of the nodes in a DAX cluster.
--
-- /See:/ 'mkParameterGroup' smart constructor.
data ParameterGroup = ParameterGroup'
  { -- | A description of the parameter group.
    description :: Lude.Maybe Lude.Text,
    -- | The name of the parameter group.
    parameterGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterGroup' with the minimum fields required to make a request.
--
-- * 'description' - A description of the parameter group.
-- * 'parameterGroupName' - The name of the parameter group.
mkParameterGroup ::
  ParameterGroup
mkParameterGroup =
  ParameterGroup'
    { description = Lude.Nothing,
      parameterGroupName = Lude.Nothing
    }

-- | A description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgDescription :: Lens.Lens' ParameterGroup (Lude.Maybe Lude.Text)
pgDescription = Lens.lens (description :: ParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ParameterGroup)
{-# DEPRECATED pgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgParameterGroupName :: Lens.Lens' ParameterGroup (Lude.Maybe Lude.Text)
pgParameterGroupName = Lens.lens (parameterGroupName :: ParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: ParameterGroup)
{-# DEPRECATED pgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.FromJSON ParameterGroup where
  parseJSON =
    Lude.withObject
      "ParameterGroup"
      ( \x ->
          ParameterGroup'
            Lude.<$> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ParameterGroupName")
      )
