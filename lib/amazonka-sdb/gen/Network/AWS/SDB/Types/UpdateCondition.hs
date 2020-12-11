-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.UpdateCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.UpdateCondition
  ( UpdateCondition (..),

    -- * Smart constructor
    mkUpdateCondition,

    -- * Lenses
    ucExists,
    ucValue,
    ucName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the conditions under which data should be updated. If an update condition is specified for a request, the data will only be updated if the condition is satisfied. For example, if an attribute with a specific name and value exists, or if a specific attribute doesn't exist.
--
-- /See:/ 'mkUpdateCondition' smart constructor.
data UpdateCondition = UpdateCondition'
  { exists ::
      Lude.Maybe Lude.Bool,
    value :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCondition' with the minimum fields required to make a request.
--
-- * 'exists' - A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
-- * 'name' - The name of the attribute involved in the condition.
-- * 'value' - The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
mkUpdateCondition ::
  UpdateCondition
mkUpdateCondition =
  UpdateCondition'
    { exists = Lude.Nothing,
      value = Lude.Nothing,
      name = Lude.Nothing
    }

-- | A value specifying whether or not the specified attribute must exist with the specified value in order for the update condition to be satisfied. Specify @true@ if the attribute must exist for the update condition to be satisfied. Specify @false@ if the attribute should not exist in order for the update condition to be satisfied.
--
-- /Note:/ Consider using 'exists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucExists :: Lens.Lens' UpdateCondition (Lude.Maybe Lude.Bool)
ucExists = Lens.lens (exists :: UpdateCondition -> Lude.Maybe Lude.Bool) (\s a -> s {exists = a} :: UpdateCondition)
{-# DEPRECATED ucExists "Use generic-lens or generic-optics with 'exists' instead." #-}

-- | The value of an attribute. This value can only be specified when the @Exists@ parameter is equal to @true@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucValue :: Lens.Lens' UpdateCondition (Lude.Maybe Lude.Text)
ucValue = Lens.lens (value :: UpdateCondition -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: UpdateCondition)
{-# DEPRECATED ucValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the attribute involved in the condition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateCondition (Lude.Maybe Lude.Text)
ucName = Lens.lens (name :: UpdateCondition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateCondition)
{-# DEPRECATED ucName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery UpdateCondition where
  toQuery UpdateCondition' {..} =
    Lude.mconcat
      [ "Exists" Lude.=: exists,
        "Value" Lude.=: value,
        "Name" Lude.=: name
      ]
