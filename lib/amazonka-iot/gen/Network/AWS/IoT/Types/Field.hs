{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Field
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Field
  ( Field (..),

    -- * Smart constructor
    mkField,

    -- * Lenses
    fName,
    fType,
  )
where

import Network.AWS.IoT.Types.FieldType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the name and data type at a field.
--
-- /See:/ 'mkField' smart constructor.
data Field = Field'
  { -- | The name of the field.
    name :: Lude.Maybe Lude.Text,
    -- | The datatype of the field.
    type' :: Lude.Maybe FieldType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Field' with the minimum fields required to make a request.
--
-- * 'name' - The name of the field.
-- * 'type'' - The datatype of the field.
mkField ::
  Field
mkField = Field' {name = Lude.Nothing, type' = Lude.Nothing}

-- | The name of the field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Field (Lude.Maybe Lude.Text)
fName = Lens.lens (name :: Field -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Field)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The datatype of the field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Field (Lude.Maybe FieldType)
fType = Lens.lens (type' :: Field -> Lude.Maybe FieldType) (\s a -> s {type' = a} :: Field)
{-# DEPRECATED fType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Field where
  parseJSON =
    Lude.withObject
      "Field"
      ( \x ->
          Field' Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON Field where
  toJSON Field' {..} =
    Lude.object
      ( Lude.catMaybes
          [("name" Lude..=) Lude.<$> name, ("type" Lude..=) Lude.<$> type']
      )
