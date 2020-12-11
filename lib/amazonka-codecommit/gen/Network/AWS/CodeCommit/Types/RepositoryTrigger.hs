-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTrigger
  ( RepositoryTrigger (..),

    -- * Smart constructor
    mkRepositoryTrigger,

    -- * Lenses
    rtBranches,
    rtCustomData,
    rtName,
    rtDestinationARN,
    rtEvents,
  )
where

import Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a trigger for a repository.
--
-- /See:/ 'mkRepositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { branches ::
      Lude.Maybe [Lude.Text],
    customData :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    destinationARN :: Lude.Text,
    events :: [RepositoryTriggerEventEnum]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RepositoryTrigger' with the minimum fields required to make a request.
--
-- * 'branches' - The branches to be included in the trigger configuration. If you specify an empty array, the trigger applies to all branches.
-- * 'customData' - Any custom data associated with the trigger to be included in the information sent to the target of the trigger.
-- * 'destinationARN' - The ARN of the resource that is the target for a trigger (for example, the ARN of a topic in Amazon SNS).
-- * 'events' - The repository events that cause the trigger to run actions in another service, such as sending a notification through Amazon SNS.
-- * 'name' - The name of the trigger.
mkRepositoryTrigger ::
  -- | 'name'
  Lude.Text ->
  -- | 'destinationARN'
  Lude.Text ->
  RepositoryTrigger
mkRepositoryTrigger pName_ pDestinationARN_ =
  RepositoryTrigger'
    { branches = Lude.Nothing,
      customData = Lude.Nothing,
      name = pName_,
      destinationARN = pDestinationARN_,
      events = Lude.mempty
    }

-- | The branches to be included in the trigger configuration. If you specify an empty array, the trigger applies to all branches.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtBranches :: Lens.Lens' RepositoryTrigger (Lude.Maybe [Lude.Text])
rtBranches = Lens.lens (branches :: RepositoryTrigger -> Lude.Maybe [Lude.Text]) (\s a -> s {branches = a} :: RepositoryTrigger)
{-# DEPRECATED rtBranches "Use generic-lens or generic-optics with 'branches' instead." #-}

-- | Any custom data associated with the trigger to be included in the information sent to the target of the trigger.
--
-- /Note:/ Consider using 'customData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCustomData :: Lens.Lens' RepositoryTrigger (Lude.Maybe Lude.Text)
rtCustomData = Lens.lens (customData :: RepositoryTrigger -> Lude.Maybe Lude.Text) (\s a -> s {customData = a} :: RepositoryTrigger)
{-# DEPRECATED rtCustomData "Use generic-lens or generic-optics with 'customData' instead." #-}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtName :: Lens.Lens' RepositoryTrigger Lude.Text
rtName = Lens.lens (name :: RepositoryTrigger -> Lude.Text) (\s a -> s {name = a} :: RepositoryTrigger)
{-# DEPRECATED rtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the resource that is the target for a trigger (for example, the ARN of a topic in Amazon SNS).
--
-- /Note:/ Consider using 'destinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDestinationARN :: Lens.Lens' RepositoryTrigger Lude.Text
rtDestinationARN = Lens.lens (destinationARN :: RepositoryTrigger -> Lude.Text) (\s a -> s {destinationARN = a} :: RepositoryTrigger)
{-# DEPRECATED rtDestinationARN "Use generic-lens or generic-optics with 'destinationARN' instead." #-}

-- | The repository events that cause the trigger to run actions in another service, such as sending a notification through Amazon SNS.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEvents :: Lens.Lens' RepositoryTrigger [RepositoryTriggerEventEnum]
rtEvents = Lens.lens (events :: RepositoryTrigger -> [RepositoryTriggerEventEnum]) (\s a -> s {events = a} :: RepositoryTrigger)
{-# DEPRECATED rtEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Lude.FromJSON RepositoryTrigger where
  parseJSON =
    Lude.withObject
      "RepositoryTrigger"
      ( \x ->
          RepositoryTrigger'
            Lude.<$> (x Lude..:? "branches" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "customData")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "destinationArn")
            Lude.<*> (x Lude..:? "events" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON RepositoryTrigger where
  toJSON RepositoryTrigger' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("branches" Lude..=) Lude.<$> branches,
            ("customData" Lude..=) Lude.<$> customData,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("destinationArn" Lude..= destinationARN),
            Lude.Just ("events" Lude..= events)
          ]
      )
