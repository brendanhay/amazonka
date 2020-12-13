{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackStatusDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackStatusDetail
  ( ConformancePackStatusDetail (..),

    -- * Smart constructor
    mkConformancePackStatusDetail,

    -- * Lenses
    cpsdConformancePackStatusReason,
    cpsdStackARN,
    cpsdLastUpdateCompletedTime,
    cpsdConformancePackName,
    cpsdLastUpdateRequestedTime,
    cpsdConformancePackId,
    cpsdConformancePackState,
    cpsdConformancePackARN,
  )
where

import Network.AWS.Config.Types.ConformancePackState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status details of a conformance pack.
--
-- /See:/ 'mkConformancePackStatusDetail' smart constructor.
data ConformancePackStatusDetail = ConformancePackStatusDetail'
  { -- | The reason of conformance pack creation failure.
    conformancePackStatusReason :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (ARN) of AWS CloudFormation stack.
    stackARN :: Lude.Text,
    -- | Last time when conformation pack creation and update was successful.
    lastUpdateCompletedTime :: Lude.Maybe Lude.Timestamp,
    -- | Name of the conformance pack.
    conformancePackName :: Lude.Text,
    -- | Last time when conformation pack creation and update was requested.
    lastUpdateRequestedTime :: Lude.Timestamp,
    -- | ID of the conformance pack.
    conformancePackId :: Lude.Text,
    -- | Indicates deployment status of conformance pack.
    --
    -- AWS Config sets the state of the conformance pack to:
    --
    --     * CREATE_IN_PROGRESS when a conformance pack creation is in progress for an account.
    --
    --
    --     * CREATE_COMPLETE when a conformance pack has been successfully created in your account.
    --
    --
    --     * CREATE_FAILED when a conformance pack creation failed in your account.
    --
    --
    --     * DELETE_IN_PROGRESS when a conformance pack deletion is in progress.
    --
    --
    --     * DELETE_FAILED when a conformance pack deletion failed in your account.
    conformancePackState :: ConformancePackState,
    -- | Amazon Resource Name (ARN) of comformance pack.
    conformancePackARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackStatusDetail' with the minimum fields required to make a request.
--
-- * 'conformancePackStatusReason' - The reason of conformance pack creation failure.
-- * 'stackARN' - Amazon Resource Name (ARN) of AWS CloudFormation stack.
-- * 'lastUpdateCompletedTime' - Last time when conformation pack creation and update was successful.
-- * 'conformancePackName' - Name of the conformance pack.
-- * 'lastUpdateRequestedTime' - Last time when conformation pack creation and update was requested.
-- * 'conformancePackId' - ID of the conformance pack.
-- * 'conformancePackState' - Indicates deployment status of conformance pack.
--
-- AWS Config sets the state of the conformance pack to:
--
--     * CREATE_IN_PROGRESS when a conformance pack creation is in progress for an account.
--
--
--     * CREATE_COMPLETE when a conformance pack has been successfully created in your account.
--
--
--     * CREATE_FAILED when a conformance pack creation failed in your account.
--
--
--     * DELETE_IN_PROGRESS when a conformance pack deletion is in progress.
--
--
--     * DELETE_FAILED when a conformance pack deletion failed in your account.
--
--
-- * 'conformancePackARN' - Amazon Resource Name (ARN) of comformance pack.
mkConformancePackStatusDetail ::
  -- | 'stackARN'
  Lude.Text ->
  -- | 'conformancePackName'
  Lude.Text ->
  -- | 'lastUpdateRequestedTime'
  Lude.Timestamp ->
  -- | 'conformancePackId'
  Lude.Text ->
  -- | 'conformancePackState'
  ConformancePackState ->
  -- | 'conformancePackARN'
  Lude.Text ->
  ConformancePackStatusDetail
mkConformancePackStatusDetail
  pStackARN_
  pConformancePackName_
  pLastUpdateRequestedTime_
  pConformancePackId_
  pConformancePackState_
  pConformancePackARN_ =
    ConformancePackStatusDetail'
      { conformancePackStatusReason =
          Lude.Nothing,
        stackARN = pStackARN_,
        lastUpdateCompletedTime = Lude.Nothing,
        conformancePackName = pConformancePackName_,
        lastUpdateRequestedTime = pLastUpdateRequestedTime_,
        conformancePackId = pConformancePackId_,
        conformancePackState = pConformancePackState_,
        conformancePackARN = pConformancePackARN_
      }

-- | The reason of conformance pack creation failure.
--
-- /Note:/ Consider using 'conformancePackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackStatusReason :: Lens.Lens' ConformancePackStatusDetail (Lude.Maybe Lude.Text)
cpsdConformancePackStatusReason = Lens.lens (conformancePackStatusReason :: ConformancePackStatusDetail -> Lude.Maybe Lude.Text) (\s a -> s {conformancePackStatusReason = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackStatusReason "Use generic-lens or generic-optics with 'conformancePackStatusReason' instead." #-}

-- | Amazon Resource Name (ARN) of AWS CloudFormation stack.
--
-- /Note:/ Consider using 'stackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdStackARN :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdStackARN = Lens.lens (stackARN :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {stackARN = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdStackARN "Use generic-lens or generic-optics with 'stackARN' instead." #-}

-- | Last time when conformation pack creation and update was successful.
--
-- /Note:/ Consider using 'lastUpdateCompletedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdLastUpdateCompletedTime :: Lens.Lens' ConformancePackStatusDetail (Lude.Maybe Lude.Timestamp)
cpsdLastUpdateCompletedTime = Lens.lens (lastUpdateCompletedTime :: ConformancePackStatusDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateCompletedTime = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdLastUpdateCompletedTime "Use generic-lens or generic-optics with 'lastUpdateCompletedTime' instead." #-}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackName :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdConformancePackName = Lens.lens (conformancePackName :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {conformancePackName = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | Last time when conformation pack creation and update was requested.
--
-- /Note:/ Consider using 'lastUpdateRequestedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdLastUpdateRequestedTime :: Lens.Lens' ConformancePackStatusDetail Lude.Timestamp
cpsdLastUpdateRequestedTime = Lens.lens (lastUpdateRequestedTime :: ConformancePackStatusDetail -> Lude.Timestamp) (\s a -> s {lastUpdateRequestedTime = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdLastUpdateRequestedTime "Use generic-lens or generic-optics with 'lastUpdateRequestedTime' instead." #-}

-- | ID of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackId :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdConformancePackId = Lens.lens (conformancePackId :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {conformancePackId = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackId "Use generic-lens or generic-optics with 'conformancePackId' instead." #-}

-- | Indicates deployment status of conformance pack.
--
-- AWS Config sets the state of the conformance pack to:
--
--     * CREATE_IN_PROGRESS when a conformance pack creation is in progress for an account.
--
--
--     * CREATE_COMPLETE when a conformance pack has been successfully created in your account.
--
--
--     * CREATE_FAILED when a conformance pack creation failed in your account.
--
--
--     * DELETE_IN_PROGRESS when a conformance pack deletion is in progress.
--
--
--     * DELETE_FAILED when a conformance pack deletion failed in your account.
--
--
--
-- /Note:/ Consider using 'conformancePackState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackState :: Lens.Lens' ConformancePackStatusDetail ConformancePackState
cpsdConformancePackState = Lens.lens (conformancePackState :: ConformancePackStatusDetail -> ConformancePackState) (\s a -> s {conformancePackState = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackState "Use generic-lens or generic-optics with 'conformancePackState' instead." #-}

-- | Amazon Resource Name (ARN) of comformance pack.
--
-- /Note:/ Consider using 'conformancePackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackARN :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdConformancePackARN = Lens.lens (conformancePackARN :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {conformancePackARN = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackARN "Use generic-lens or generic-optics with 'conformancePackARN' instead." #-}

instance Lude.FromJSON ConformancePackStatusDetail where
  parseJSON =
    Lude.withObject
      "ConformancePackStatusDetail"
      ( \x ->
          ConformancePackStatusDetail'
            Lude.<$> (x Lude..:? "ConformancePackStatusReason")
            Lude.<*> (x Lude..: "StackArn")
            Lude.<*> (x Lude..:? "LastUpdateCompletedTime")
            Lude.<*> (x Lude..: "ConformancePackName")
            Lude.<*> (x Lude..: "LastUpdateRequestedTime")
            Lude.<*> (x Lude..: "ConformancePackId")
            Lude.<*> (x Lude..: "ConformancePackState")
            Lude.<*> (x Lude..: "ConformancePackArn")
      )
