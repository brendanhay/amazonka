{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionStatus
  ( OptionStatus (..),

    -- * Smart constructor
    mkOptionStatus,

    -- * Lenses
    osState,
    osUpdateDate,
    osPendingDeletion,
    osCreationDate,
    osUpdateVersion,
  )
where

import Network.AWS.ElasticSearch.Types.OptionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the current status of the entity.
--
-- /See:/ 'mkOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | Provides the @OptionState@ for the Elasticsearch domain.
    state :: OptionState,
    -- | Timestamp which tells the last updated time for the entity.
    updateDate :: Lude.Timestamp,
    -- | Indicates whether the Elasticsearch domain is being deleted.
    pendingDeletion :: Lude.Maybe Lude.Bool,
    -- | Timestamp which tells the creation date for the entity.
    creationDate :: Lude.Timestamp,
    -- | Specifies the latest version for the entity.
    updateVersion :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- * 'state' - Provides the @OptionState@ for the Elasticsearch domain.
-- * 'updateDate' - Timestamp which tells the last updated time for the entity.
-- * 'pendingDeletion' - Indicates whether the Elasticsearch domain is being deleted.
-- * 'creationDate' - Timestamp which tells the creation date for the entity.
-- * 'updateVersion' - Specifies the latest version for the entity.
mkOptionStatus ::
  -- | 'state'
  OptionState ->
  -- | 'updateDate'
  Lude.Timestamp ->
  -- | 'creationDate'
  Lude.Timestamp ->
  OptionStatus
mkOptionStatus pState_ pUpdateDate_ pCreationDate_ =
  OptionStatus'
    { state = pState_,
      updateDate = pUpdateDate_,
      pendingDeletion = Lude.Nothing,
      creationDate = pCreationDate_,
      updateVersion = Lude.Nothing
    }

-- | Provides the @OptionState@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OptionStatus OptionState
osState = Lens.lens (state :: OptionStatus -> OptionState) (\s a -> s {state = a} :: OptionStatus)
{-# DEPRECATED osState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Timestamp which tells the last updated time for the entity.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateDate :: Lens.Lens' OptionStatus Lude.Timestamp
osUpdateDate = Lens.lens (updateDate :: OptionStatus -> Lude.Timestamp) (\s a -> s {updateDate = a} :: OptionStatus)
{-# DEPRECATED osUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | Indicates whether the Elasticsearch domain is being deleted.
--
-- /Note:/ Consider using 'pendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPendingDeletion :: Lens.Lens' OptionStatus (Lude.Maybe Lude.Bool)
osPendingDeletion = Lens.lens (pendingDeletion :: OptionStatus -> Lude.Maybe Lude.Bool) (\s a -> s {pendingDeletion = a} :: OptionStatus)
{-# DEPRECATED osPendingDeletion "Use generic-lens or generic-optics with 'pendingDeletion' instead." #-}

-- | Timestamp which tells the creation date for the entity.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCreationDate :: Lens.Lens' OptionStatus Lude.Timestamp
osCreationDate = Lens.lens (creationDate :: OptionStatus -> Lude.Timestamp) (\s a -> s {creationDate = a} :: OptionStatus)
{-# DEPRECATED osCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Specifies the latest version for the entity.
--
-- /Note:/ Consider using 'updateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateVersion :: Lens.Lens' OptionStatus (Lude.Maybe Lude.Natural)
osUpdateVersion = Lens.lens (updateVersion :: OptionStatus -> Lude.Maybe Lude.Natural) (\s a -> s {updateVersion = a} :: OptionStatus)
{-# DEPRECATED osUpdateVersion "Use generic-lens or generic-optics with 'updateVersion' instead." #-}

instance Lude.FromJSON OptionStatus where
  parseJSON =
    Lude.withObject
      "OptionStatus"
      ( \x ->
          OptionStatus'
            Lude.<$> (x Lude..: "State")
            Lude.<*> (x Lude..: "UpdateDate")
            Lude.<*> (x Lude..:? "PendingDeletion")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..:? "UpdateVersion")
      )
