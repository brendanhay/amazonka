-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateMachineListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.StateMachineListItem
  ( StateMachineListItem (..),

    -- * Smart constructor
    mkStateMachineListItem,

    -- * Lenses
    smliStateMachineARN,
    smliName,
    smliType,
    smliCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.StateMachineType

-- | Contains details about the state machine.
--
-- /See:/ 'mkStateMachineListItem' smart constructor.
data StateMachineListItem = StateMachineListItem'
  { stateMachineARN ::
      Lude.Text,
    name :: Lude.Text,
    type' :: StateMachineType,
    creationDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StateMachineListItem' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date the state machine is created.
-- * 'name' - The name of the state machine.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) that identifies the state machine.
-- * 'type'' -
mkStateMachineListItem ::
  -- | 'stateMachineARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  StateMachineType ->
  -- | 'creationDate'
  Lude.Timestamp ->
  StateMachineListItem
mkStateMachineListItem
  pStateMachineARN_
  pName_
  pType_
  pCreationDate_ =
    StateMachineListItem'
      { stateMachineARN = pStateMachineARN_,
        name = pName_,
        type' = pType_,
        creationDate = pCreationDate_
      }

-- | The Amazon Resource Name (ARN) that identifies the state machine.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smliStateMachineARN :: Lens.Lens' StateMachineListItem Lude.Text
smliStateMachineARN = Lens.lens (stateMachineARN :: StateMachineListItem -> Lude.Text) (\s a -> s {stateMachineARN = a} :: StateMachineListItem)
{-# DEPRECATED smliStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

-- | The name of the state machine.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@
--
--
--     * wildcard characters @? *@
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smliName :: Lens.Lens' StateMachineListItem Lude.Text
smliName = Lens.lens (name :: StateMachineListItem -> Lude.Text) (\s a -> s {name = a} :: StateMachineListItem)
{-# DEPRECATED smliName "Use generic-lens or generic-optics with 'name' instead." #-}

-- |
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smliType :: Lens.Lens' StateMachineListItem StateMachineType
smliType = Lens.lens (type' :: StateMachineListItem -> StateMachineType) (\s a -> s {type' = a} :: StateMachineListItem)
{-# DEPRECATED smliType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date the state machine is created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smliCreationDate :: Lens.Lens' StateMachineListItem Lude.Timestamp
smliCreationDate = Lens.lens (creationDate :: StateMachineListItem -> Lude.Timestamp) (\s a -> s {creationDate = a} :: StateMachineListItem)
{-# DEPRECATED smliCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON StateMachineListItem where
  parseJSON =
    Lude.withObject
      "StateMachineListItem"
      ( \x ->
          StateMachineListItem'
            Lude.<$> (x Lude..: "stateMachineArn")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..: "creationDate")
      )
