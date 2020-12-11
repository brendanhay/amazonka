-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ExecutionParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ExecutionParameter
  ( ExecutionParameter (..),

    -- * Smart constructor
    mkExecutionParameter,

    -- * Lenses
    epDefaultValues,
    epName,
    epType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of an execution parameter value that is passed to a self-service action when executed on a provisioned product.
--
-- /See:/ 'mkExecutionParameter' smart constructor.
data ExecutionParameter = ExecutionParameter'
  { defaultValues ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionParameter' with the minimum fields required to make a request.
--
-- * 'defaultValues' - The default values for the execution parameter.
-- * 'name' - The name of the execution parameter.
-- * 'type'' - The execution parameter type.
mkExecutionParameter ::
  ExecutionParameter
mkExecutionParameter =
  ExecutionParameter'
    { defaultValues = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The default values for the execution parameter.
--
-- /Note:/ Consider using 'defaultValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epDefaultValues :: Lens.Lens' ExecutionParameter (Lude.Maybe [Lude.Text])
epDefaultValues = Lens.lens (defaultValues :: ExecutionParameter -> Lude.Maybe [Lude.Text]) (\s a -> s {defaultValues = a} :: ExecutionParameter)
{-# DEPRECATED epDefaultValues "Use generic-lens or generic-optics with 'defaultValues' instead." #-}

-- | The name of the execution parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epName :: Lens.Lens' ExecutionParameter (Lude.Maybe Lude.Text)
epName = Lens.lens (name :: ExecutionParameter -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ExecutionParameter)
{-# DEPRECATED epName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The execution parameter type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epType :: Lens.Lens' ExecutionParameter (Lude.Maybe Lude.Text)
epType = Lens.lens (type' :: ExecutionParameter -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ExecutionParameter)
{-# DEPRECATED epType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ExecutionParameter where
  parseJSON =
    Lude.withObject
      "ExecutionParameter"
      ( \x ->
          ExecutionParameter'
            Lude.<$> (x Lude..:? "DefaultValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
      )
