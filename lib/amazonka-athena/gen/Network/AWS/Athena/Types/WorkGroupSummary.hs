{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupSummary
  ( WorkGroupSummary (..),

    -- * Smart constructor
    mkWorkGroupSummary,

    -- * Lenses
    wgsCreationTime,
    wgsState,
    wgsName,
    wgsDescription,
  )
where

import Network.AWS.Athena.Types.WorkGroupState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary information for the workgroup, which includes its name, state, description, and the date and time it was created.
--
-- /See:/ 'mkWorkGroupSummary' smart constructor.
data WorkGroupSummary = WorkGroupSummary'
  { -- | The workgroup creation date and time.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The state of the workgroup.
    state :: Lude.Maybe WorkGroupState,
    -- | The name of the workgroup.
    name :: Lude.Maybe Lude.Text,
    -- | The workgroup description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkGroupSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The workgroup creation date and time.
-- * 'state' - The state of the workgroup.
-- * 'name' - The name of the workgroup.
-- * 'description' - The workgroup description.
mkWorkGroupSummary ::
  WorkGroupSummary
mkWorkGroupSummary =
  WorkGroupSummary'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The workgroup creation date and time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsCreationTime :: Lens.Lens' WorkGroupSummary (Lude.Maybe Lude.Timestamp)
wgsCreationTime = Lens.lens (creationTime :: WorkGroupSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: WorkGroupSummary)
{-# DEPRECATED wgsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the workgroup.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsState :: Lens.Lens' WorkGroupSummary (Lude.Maybe WorkGroupState)
wgsState = Lens.lens (state :: WorkGroupSummary -> Lude.Maybe WorkGroupState) (\s a -> s {state = a} :: WorkGroupSummary)
{-# DEPRECATED wgsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the workgroup.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsName :: Lens.Lens' WorkGroupSummary (Lude.Maybe Lude.Text)
wgsName = Lens.lens (name :: WorkGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WorkGroupSummary)
{-# DEPRECATED wgsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgsDescription :: Lens.Lens' WorkGroupSummary (Lude.Maybe Lude.Text)
wgsDescription = Lens.lens (description :: WorkGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: WorkGroupSummary)
{-# DEPRECATED wgsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON WorkGroupSummary where
  parseJSON =
    Lude.withObject
      "WorkGroupSummary"
      ( \x ->
          WorkGroupSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
      )
