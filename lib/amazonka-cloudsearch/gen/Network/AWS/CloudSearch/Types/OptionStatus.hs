-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.OptionStatus
  ( OptionStatus (..),

    -- * Smart constructor
    mkOptionStatus,

    -- * Lenses
    osPendingDeletion,
    osUpdateVersion,
    osCreationDate,
    osUpdateDate,
    osState,
  )
where

import Network.AWS.CloudSearch.Types.OptionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of domain configuration option.
--
-- /See:/ 'mkOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { pendingDeletion ::
      Lude.Maybe Lude.Bool,
    updateVersion :: Lude.Maybe Lude.Natural,
    creationDate :: Lude.ISO8601,
    updateDate :: Lude.ISO8601,
    state :: OptionState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- * 'creationDate' - A timestamp for when this option was created.
-- * 'pendingDeletion' - Indicates that the option will be deleted once processing is complete.
-- * 'state' - The state of processing a change to an option. Possible values:
--
--
--     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.
--
--     * @Processing@ : the option's latest value is in the process of being activated.
--
--     * @Active@ : the option's latest value is completely deployed.
--
--     * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
--
-- * 'updateDate' - A timestamp for when this option was last updated.
-- * 'updateVersion' - A unique integer that indicates when this option was last updated.
mkOptionStatus ::
  -- | 'creationDate'
  Lude.ISO8601 ->
  -- | 'updateDate'
  Lude.ISO8601 ->
  -- | 'state'
  OptionState ->
  OptionStatus
mkOptionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { pendingDeletion = Lude.Nothing,
      updateVersion = Lude.Nothing,
      creationDate = pCreationDate_,
      updateDate = pUpdateDate_,
      state = pState_
    }

-- | Indicates that the option will be deleted once processing is complete.
--
-- /Note:/ Consider using 'pendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPendingDeletion :: Lens.Lens' OptionStatus (Lude.Maybe Lude.Bool)
osPendingDeletion = Lens.lens (pendingDeletion :: OptionStatus -> Lude.Maybe Lude.Bool) (\s a -> s {pendingDeletion = a} :: OptionStatus)
{-# DEPRECATED osPendingDeletion "Use generic-lens or generic-optics with 'pendingDeletion' instead." #-}

-- | A unique integer that indicates when this option was last updated.
--
-- /Note:/ Consider using 'updateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateVersion :: Lens.Lens' OptionStatus (Lude.Maybe Lude.Natural)
osUpdateVersion = Lens.lens (updateVersion :: OptionStatus -> Lude.Maybe Lude.Natural) (\s a -> s {updateVersion = a} :: OptionStatus)
{-# DEPRECATED osUpdateVersion "Use generic-lens or generic-optics with 'updateVersion' instead." #-}

-- | A timestamp for when this option was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCreationDate :: Lens.Lens' OptionStatus Lude.ISO8601
osCreationDate = Lens.lens (creationDate :: OptionStatus -> Lude.ISO8601) (\s a -> s {creationDate = a} :: OptionStatus)
{-# DEPRECATED osCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A timestamp for when this option was last updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateDate :: Lens.Lens' OptionStatus Lude.ISO8601
osUpdateDate = Lens.lens (updateDate :: OptionStatus -> Lude.ISO8601) (\s a -> s {updateDate = a} :: OptionStatus)
{-# DEPRECATED osUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | The state of processing a change to an option. Possible values:
--
--
--     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.
--
--     * @Processing@ : the option's latest value is in the process of being activated.
--
--     * @Active@ : the option's latest value is completely deployed.
--
--     * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OptionStatus OptionState
osState = Lens.lens (state :: OptionStatus -> OptionState) (\s a -> s {state = a} :: OptionStatus)
{-# DEPRECATED osState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.FromXML OptionStatus where
  parseXML x =
    OptionStatus'
      Lude.<$> (x Lude..@? "PendingDeletion")
      Lude.<*> (x Lude..@? "UpdateVersion")
      Lude.<*> (x Lude..@ "CreationDate")
      Lude.<*> (x Lude..@ "UpdateDate")
      Lude.<*> (x Lude..@ "State")
