{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PropertyNameQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PropertyNameQuery
  ( PropertyNameQuery (..),

    -- * Smart constructor
    mkPropertyNameQuery,

    -- * Lenses
    pnqPropertyNameHint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Part of the @SuggestionQuery@ type. Specifies a hint for retrieving property names that begin with the specified text.
--
-- /See:/ 'mkPropertyNameQuery' smart constructor.
newtype PropertyNameQuery = PropertyNameQuery'
  { -- | Text that begins a property's name.
    propertyNameHint :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PropertyNameQuery' with the minimum fields required to make a request.
--
-- * 'propertyNameHint' - Text that begins a property's name.
mkPropertyNameQuery ::
  -- | 'propertyNameHint'
  Lude.Text ->
  PropertyNameQuery
mkPropertyNameQuery pPropertyNameHint_ =
  PropertyNameQuery' {propertyNameHint = pPropertyNameHint_}

-- | Text that begins a property's name.
--
-- /Note:/ Consider using 'propertyNameHint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnqPropertyNameHint :: Lens.Lens' PropertyNameQuery Lude.Text
pnqPropertyNameHint = Lens.lens (propertyNameHint :: PropertyNameQuery -> Lude.Text) (\s a -> s {propertyNameHint = a} :: PropertyNameQuery)
{-# DEPRECATED pnqPropertyNameHint "Use generic-lens or generic-optics with 'propertyNameHint' instead." #-}

instance Lude.ToJSON PropertyNameQuery where
  toJSON PropertyNameQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("PropertyNameHint" Lude..= propertyNameHint)]
      )
