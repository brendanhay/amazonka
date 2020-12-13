{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    osState,
    osUpdateDate,
    osPendingDeletion,
    osCreationDate,
    osUpdateVersion,
  )
where

import Network.AWS.CloudSearch.Types.OptionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of domain configuration option.
--
-- /See:/ 'mkOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | The state of processing a change to an option. Possible values:
    --
    --
    --     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.
    --
    --     * @Processing@ : the option's latest value is in the process of being activated.
    --
    --     * @Active@ : the option's latest value is completely deployed.
    --
    --     * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
    state :: OptionState,
    -- | A timestamp for when this option was last updated.
    updateDate :: Lude.DateTime,
    -- | Indicates that the option will be deleted once processing is complete.
    pendingDeletion :: Lude.Maybe Lude.Bool,
    -- | A timestamp for when this option was created.
    creationDate :: Lude.DateTime,
    -- | A unique integer that indicates when this option was last updated.
    updateVersion :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
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
-- * 'pendingDeletion' - Indicates that the option will be deleted once processing is complete.
-- * 'creationDate' - A timestamp for when this option was created.
-- * 'updateVersion' - A unique integer that indicates when this option was last updated.
mkOptionStatus ::
  -- | 'state'
  OptionState ->
  -- | 'updateDate'
  Lude.DateTime ->
  -- | 'creationDate'
  Lude.DateTime ->
  OptionStatus
mkOptionStatus pState_ pUpdateDate_ pCreationDate_ =
  OptionStatus'
    { state = pState_,
      updateDate = pUpdateDate_,
      pendingDeletion = Lude.Nothing,
      creationDate = pCreationDate_,
      updateVersion = Lude.Nothing
    }

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

-- | A timestamp for when this option was last updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateDate :: Lens.Lens' OptionStatus Lude.DateTime
osUpdateDate = Lens.lens (updateDate :: OptionStatus -> Lude.DateTime) (\s a -> s {updateDate = a} :: OptionStatus)
{-# DEPRECATED osUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | Indicates that the option will be deleted once processing is complete.
--
-- /Note:/ Consider using 'pendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPendingDeletion :: Lens.Lens' OptionStatus (Lude.Maybe Lude.Bool)
osPendingDeletion = Lens.lens (pendingDeletion :: OptionStatus -> Lude.Maybe Lude.Bool) (\s a -> s {pendingDeletion = a} :: OptionStatus)
{-# DEPRECATED osPendingDeletion "Use generic-lens or generic-optics with 'pendingDeletion' instead." #-}

-- | A timestamp for when this option was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCreationDate :: Lens.Lens' OptionStatus Lude.DateTime
osCreationDate = Lens.lens (creationDate :: OptionStatus -> Lude.DateTime) (\s a -> s {creationDate = a} :: OptionStatus)
{-# DEPRECATED osCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A unique integer that indicates when this option was last updated.
--
-- /Note:/ Consider using 'updateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateVersion :: Lens.Lens' OptionStatus (Lude.Maybe Lude.Natural)
osUpdateVersion = Lens.lens (updateVersion :: OptionStatus -> Lude.Maybe Lude.Natural) (\s a -> s {updateVersion = a} :: OptionStatus)
{-# DEPRECATED osUpdateVersion "Use generic-lens or generic-optics with 'updateVersion' instead." #-}

instance Lude.FromXML OptionStatus where
  parseXML x =
    OptionStatus'
      Lude.<$> (x Lude..@ "State")
      Lude.<*> (x Lude..@ "UpdateDate")
      Lude.<*> (x Lude..@? "PendingDeletion")
      Lude.<*> (x Lude..@ "CreationDate")
      Lude.<*> (x Lude..@? "UpdateVersion")
