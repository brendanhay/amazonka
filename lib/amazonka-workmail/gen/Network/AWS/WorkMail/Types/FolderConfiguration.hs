{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.FolderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.FolderConfiguration
  ( FolderConfiguration (..),

    -- * Smart constructor
    mkFolderConfiguration,

    -- * Lenses
    fcPeriod,
    fcName,
    fcAction,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.FolderName
import Network.AWS.WorkMail.Types.RetentionAction

-- | The configuration applied to an organization's folders by its retention policy.
--
-- /See:/ 'mkFolderConfiguration' smart constructor.
data FolderConfiguration = FolderConfiguration'
  { period ::
      Lude.Maybe Lude.Natural,
    name :: FolderName,
    action :: RetentionAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FolderConfiguration' with the minimum fields required to make a request.
--
-- * 'action' - The action to take on the folder contents at the end of the folder configuration period.
-- * 'name' - The folder name.
-- * 'period' - The period of time at which the folder configuration action is applied.
mkFolderConfiguration ::
  -- | 'name'
  FolderName ->
  -- | 'action'
  RetentionAction ->
  FolderConfiguration
mkFolderConfiguration pName_ pAction_ =
  FolderConfiguration'
    { period = Lude.Nothing,
      name = pName_,
      action = pAction_
    }

-- | The period of time at which the folder configuration action is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcPeriod :: Lens.Lens' FolderConfiguration (Lude.Maybe Lude.Natural)
fcPeriod = Lens.lens (period :: FolderConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {period = a} :: FolderConfiguration)
{-# DEPRECATED fcPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The folder name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcName :: Lens.Lens' FolderConfiguration FolderName
fcName = Lens.lens (name :: FolderConfiguration -> FolderName) (\s a -> s {name = a} :: FolderConfiguration)
{-# DEPRECATED fcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The action to take on the folder contents at the end of the folder configuration period.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcAction :: Lens.Lens' FolderConfiguration RetentionAction
fcAction = Lens.lens (action :: FolderConfiguration -> RetentionAction) (\s a -> s {action = a} :: FolderConfiguration)
{-# DEPRECATED fcAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.FromJSON FolderConfiguration where
  parseJSON =
    Lude.withObject
      "FolderConfiguration"
      ( \x ->
          FolderConfiguration'
            Lude.<$> (x Lude..:? "Period")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Action")
      )

instance Lude.ToJSON FolderConfiguration where
  toJSON FolderConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Period" Lude..=) Lude.<$> period,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Action" Lude..= action)
          ]
      )
