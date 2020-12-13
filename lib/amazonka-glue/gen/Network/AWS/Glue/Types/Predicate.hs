{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Predicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Predicate
  ( Predicate (..),

    -- * Smart constructor
    mkPredicate,

    -- * Lenses
    pLogical,
    pConditions,
  )
where

import Network.AWS.Glue.Types.Condition
import Network.AWS.Glue.Types.Logical
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the predicate of the trigger, which determines when it fires.
--
-- /See:/ 'mkPredicate' smart constructor.
data Predicate = Predicate'
  { -- | An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
    logical :: Lude.Maybe Logical,
    -- | A list of the conditions that determine when the trigger will fire.
    conditions :: Lude.Maybe [Condition]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Predicate' with the minimum fields required to make a request.
--
-- * 'logical' - An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
-- * 'conditions' - A list of the conditions that determine when the trigger will fire.
mkPredicate ::
  Predicate
mkPredicate =
  Predicate' {logical = Lude.Nothing, conditions = Lude.Nothing}

-- | An optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
--
-- /Note:/ Consider using 'logical' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLogical :: Lens.Lens' Predicate (Lude.Maybe Logical)
pLogical = Lens.lens (logical :: Predicate -> Lude.Maybe Logical) (\s a -> s {logical = a} :: Predicate)
{-# DEPRECATED pLogical "Use generic-lens or generic-optics with 'logical' instead." #-}

-- | A list of the conditions that determine when the trigger will fire.
--
-- /Note:/ Consider using 'conditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pConditions :: Lens.Lens' Predicate (Lude.Maybe [Condition])
pConditions = Lens.lens (conditions :: Predicate -> Lude.Maybe [Condition]) (\s a -> s {conditions = a} :: Predicate)
{-# DEPRECATED pConditions "Use generic-lens or generic-optics with 'conditions' instead." #-}

instance Lude.FromJSON Predicate where
  parseJSON =
    Lude.withObject
      "Predicate"
      ( \x ->
          Predicate'
            Lude.<$> (x Lude..:? "Logical")
            Lude.<*> (x Lude..:? "Conditions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON Predicate where
  toJSON Predicate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Logical" Lude..=) Lude.<$> logical,
            ("Conditions" Lude..=) Lude.<$> conditions
          ]
      )
