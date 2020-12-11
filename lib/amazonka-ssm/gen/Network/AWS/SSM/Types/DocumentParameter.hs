-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentParameter
  ( DocumentParameter (..),

    -- * Smart constructor
    mkDocumentParameter,

    -- * Lenses
    dpName,
    dpDefaultValue,
    dpType,
    dpDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.DocumentParameterType

-- | Parameters specified in a System Manager document that run on the server when the command is run.
--
-- /See:/ 'mkDocumentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
  { name ::
      Lude.Maybe Lude.Text,
    defaultValue :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe DocumentParameterType,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentParameter' with the minimum fields required to make a request.
--
-- * 'defaultValue' - If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
-- * 'description' - A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
-- * 'name' - The name of the parameter.
-- * 'type'' - The type of parameter. The type can be either String or StringList.
mkDocumentParameter ::
  DocumentParameter
mkDocumentParameter =
  DocumentParameter'
    { name = Lude.Nothing,
      defaultValue = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DocumentParameter (Lude.Maybe Lude.Text)
dpName = Lens.lens (name :: DocumentParameter -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentParameter)
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDefaultValue :: Lens.Lens' DocumentParameter (Lude.Maybe Lude.Text)
dpDefaultValue = Lens.lens (defaultValue :: DocumentParameter -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: DocumentParameter)
{-# DEPRECATED dpDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | The type of parameter. The type can be either String or StringList.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpType :: Lens.Lens' DocumentParameter (Lude.Maybe DocumentParameterType)
dpType = Lens.lens (type' :: DocumentParameter -> Lude.Maybe DocumentParameterType) (\s a -> s {type' = a} :: DocumentParameter)
{-# DEPRECATED dpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDescription :: Lens.Lens' DocumentParameter (Lude.Maybe Lude.Text)
dpDescription = Lens.lens (description :: DocumentParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DocumentParameter)
{-# DEPRECATED dpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON DocumentParameter where
  parseJSON =
    Lude.withObject
      "DocumentParameter"
      ( \x ->
          DocumentParameter'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "DefaultValue")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
      )
