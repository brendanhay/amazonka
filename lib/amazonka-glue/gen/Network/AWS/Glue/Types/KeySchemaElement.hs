{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.KeySchemaElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.KeySchemaElement
  ( KeySchemaElement (..),

    -- * Smart constructor
    mkKeySchemaElement,

    -- * Lenses
    kseName,
    kseType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A partition key pair consisting of a name and a type.
--
-- /See:/ 'mkKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The name of a partition key.
    name :: Lude.Text,
    -- | The type of a partition key.
    type' :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeySchemaElement' with the minimum fields required to make a request.
--
-- * 'name' - The name of a partition key.
-- * 'type'' - The type of a partition key.
mkKeySchemaElement ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  KeySchemaElement
mkKeySchemaElement pName_ pType_ =
  KeySchemaElement' {name = pName_, type' = pType_}

-- | The name of a partition key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseName :: Lens.Lens' KeySchemaElement Lude.Text
kseName = Lens.lens (name :: KeySchemaElement -> Lude.Text) (\s a -> s {name = a} :: KeySchemaElement)
{-# DEPRECATED kseName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of a partition key.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseType :: Lens.Lens' KeySchemaElement Lude.Text
kseType = Lens.lens (type' :: KeySchemaElement -> Lude.Text) (\s a -> s {type' = a} :: KeySchemaElement)
{-# DEPRECATED kseType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON KeySchemaElement where
  parseJSON =
    Lude.withObject
      "KeySchemaElement"
      ( \x ->
          KeySchemaElement'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "Type")
      )
