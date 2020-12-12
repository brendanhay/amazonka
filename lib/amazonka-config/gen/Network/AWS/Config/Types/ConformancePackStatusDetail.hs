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
    cpsdLastUpdateCompletedTime,
    cpsdConformancePackName,
    cpsdConformancePackId,
    cpsdConformancePackARN,
    cpsdConformancePackState,
    cpsdStackARN,
    cpsdLastUpdateRequestedTime,
  )
where

import Network.AWS.Config.Types.ConformancePackState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status details of a conformance pack.
--
-- /See:/ 'mkConformancePackStatusDetail' smart constructor.
data ConformancePackStatusDetail = ConformancePackStatusDetail'
  { conformancePackStatusReason ::
      Lude.Maybe Lude.Text,
    lastUpdateCompletedTime ::
      Lude.Maybe Lude.Timestamp,
    conformancePackName :: Lude.Text,
    conformancePackId :: Lude.Text,
    conformancePackARN :: Lude.Text,
    conformancePackState ::
      ConformancePackState,
    stackARN :: Lude.Text,
    lastUpdateRequestedTime ::
      Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackStatusDetail' with the minimum fields required to make a request.
--
-- * 'conformancePackARN' - Amazon Resource Name (ARN) of comformance pack.
-- * 'conformancePackId' - ID of the conformance pack.
-- * 'conformancePackName' - Name of the conformance pack.
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
-- * 'conformancePackStatusReason' - The reason of conformance pack creation failure.
-- * 'lastUpdateCompletedTime' - Last time when conformation pack creation and update was successful.
-- * 'lastUpdateRequestedTime' - Last time when conformation pack creation and update was requested.
-- * 'stackARN' - Amazon Resource Name (ARN) of AWS CloudFormation stack.
mkConformancePackStatusDetail ::
  -- | 'conformancePackName'
  Lude.Text ->
  -- | 'conformancePackId'
  Lude.Text ->
  -- | 'conformancePackARN'
  Lude.Text ->
  -- | 'conformancePackState'
  ConformancePackState ->
  -- | 'stackARN'
  Lude.Text ->
  -- | 'lastUpdateRequestedTime'
  Lude.Timestamp ->
  ConformancePackStatusDetail
mkConformancePackStatusDetail
  pConformancePackName_
  pConformancePackId_
  pConformancePackARN_
  pConformancePackState_
  pStackARN_
  pLastUpdateRequestedTime_ =
    ConformancePackStatusDetail'
      { conformancePackStatusReason =
          Lude.Nothing,
        lastUpdateCompletedTime = Lude.Nothing,
        conformancePackName = pConformancePackName_,
        conformancePackId = pConformancePackId_,
        conformancePackARN = pConformancePackARN_,
        conformancePackState = pConformancePackState_,
        stackARN = pStackARN_,
        lastUpdateRequestedTime = pLastUpdateRequestedTime_
      }

-- | The reason of conformance pack creation failure.
--
-- /Note:/ Consider using 'conformancePackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackStatusReason :: Lens.Lens' ConformancePackStatusDetail (Lude.Maybe Lude.Text)
cpsdConformancePackStatusReason = Lens.lens (conformancePackStatusReason :: ConformancePackStatusDetail -> Lude.Maybe Lude.Text) (\s a -> s {conformancePackStatusReason = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackStatusReason "Use generic-lens or generic-optics with 'conformancePackStatusReason' instead." #-}

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

-- | ID of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackId :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdConformancePackId = Lens.lens (conformancePackId :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {conformancePackId = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackId "Use generic-lens or generic-optics with 'conformancePackId' instead." #-}

-- | Amazon Resource Name (ARN) of comformance pack.
--
-- /Note:/ Consider using 'conformancePackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackARN :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdConformancePackARN = Lens.lens (conformancePackARN :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {conformancePackARN = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdConformancePackARN "Use generic-lens or generic-optics with 'conformancePackARN' instead." #-}

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

-- | Amazon Resource Name (ARN) of AWS CloudFormation stack.
--
-- /Note:/ Consider using 'stackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdStackARN :: Lens.Lens' ConformancePackStatusDetail Lude.Text
cpsdStackARN = Lens.lens (stackARN :: ConformancePackStatusDetail -> Lude.Text) (\s a -> s {stackARN = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdStackARN "Use generic-lens or generic-optics with 'stackARN' instead." #-}

-- | Last time when conformation pack creation and update was requested.
--
-- /Note:/ Consider using 'lastUpdateRequestedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdLastUpdateRequestedTime :: Lens.Lens' ConformancePackStatusDetail Lude.Timestamp
cpsdLastUpdateRequestedTime = Lens.lens (lastUpdateRequestedTime :: ConformancePackStatusDetail -> Lude.Timestamp) (\s a -> s {lastUpdateRequestedTime = a} :: ConformancePackStatusDetail)
{-# DEPRECATED cpsdLastUpdateRequestedTime "Use generic-lens or generic-optics with 'lastUpdateRequestedTime' instead." #-}

instance Lude.FromJSON ConformancePackStatusDetail where
  parseJSON =
    Lude.withObject
      "ConformancePackStatusDetail"
      ( \x ->
          ConformancePackStatusDetail'
            Lude.<$> (x Lude..:? "ConformancePackStatusReason")
            Lude.<*> (x Lude..:? "LastUpdateCompletedTime")
            Lude.<*> (x Lude..: "ConformancePackName")
            Lude.<*> (x Lude..: "ConformancePackId")
            Lude.<*> (x Lude..: "ConformancePackArn")
            Lude.<*> (x Lude..: "ConformancePackState")
            Lude.<*> (x Lude..: "StackArn")
            Lude.<*> (x Lude..: "LastUpdateRequestedTime")
      )
