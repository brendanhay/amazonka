{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackStatusDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ConformancePackStatusDetail
  ( ConformancePackStatusDetail (..)
  -- * Smart constructor
  , mkConformancePackStatusDetail
  -- * Lenses
  , cpsdConformancePackName
  , cpsdConformancePackId
  , cpsdConformancePackArn
  , cpsdConformancePackState
  , cpsdStackArn
  , cpsdLastUpdateRequestedTime
  , cpsdConformancePackStatusReason
  , cpsdLastUpdateCompletedTime
  ) where

import qualified Network.AWS.Config.Types.ConformancePackArn as Types
import qualified Network.AWS.Config.Types.ConformancePackId as Types
import qualified Network.AWS.Config.Types.ConformancePackName as Types
import qualified Network.AWS.Config.Types.ConformancePackState as Types
import qualified Network.AWS.Config.Types.ConformancePackStatusReason as Types
import qualified Network.AWS.Config.Types.StackArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status details of a conformance pack.
--
-- /See:/ 'mkConformancePackStatusDetail' smart constructor.
data ConformancePackStatusDetail = ConformancePackStatusDetail'
  { conformancePackName :: Types.ConformancePackName
    -- ^ Name of the conformance pack.
  , conformancePackId :: Types.ConformancePackId
    -- ^ ID of the conformance pack.
  , conformancePackArn :: Types.ConformancePackArn
    -- ^ Amazon Resource Name (ARN) of comformance pack.
  , conformancePackState :: Types.ConformancePackState
    -- ^ Indicates deployment status of conformance pack.
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
  , stackArn :: Types.StackArn
    -- ^ Amazon Resource Name (ARN) of AWS CloudFormation stack. 
  , lastUpdateRequestedTime :: Core.NominalDiffTime
    -- ^ Last time when conformation pack creation and update was requested.
  , conformancePackStatusReason :: Core.Maybe Types.ConformancePackStatusReason
    -- ^ The reason of conformance pack creation failure.
  , lastUpdateCompletedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Last time when conformation pack creation and update was successful.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConformancePackStatusDetail' value with any optional fields omitted.
mkConformancePackStatusDetail
    :: Types.ConformancePackName -- ^ 'conformancePackName'
    -> Types.ConformancePackId -- ^ 'conformancePackId'
    -> Types.ConformancePackArn -- ^ 'conformancePackArn'
    -> Types.ConformancePackState -- ^ 'conformancePackState'
    -> Types.StackArn -- ^ 'stackArn'
    -> Core.NominalDiffTime -- ^ 'lastUpdateRequestedTime'
    -> ConformancePackStatusDetail
mkConformancePackStatusDetail conformancePackName conformancePackId
  conformancePackArn conformancePackState stackArn
  lastUpdateRequestedTime
  = ConformancePackStatusDetail'{conformancePackName,
                                 conformancePackId, conformancePackArn, conformancePackState,
                                 stackArn, lastUpdateRequestedTime,
                                 conformancePackStatusReason = Core.Nothing,
                                 lastUpdateCompletedTime = Core.Nothing}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackName :: Lens.Lens' ConformancePackStatusDetail Types.ConformancePackName
cpsdConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE cpsdConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

-- | ID of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackId :: Lens.Lens' ConformancePackStatusDetail Types.ConformancePackId
cpsdConformancePackId = Lens.field @"conformancePackId"
{-# INLINEABLE cpsdConformancePackId #-}
{-# DEPRECATED conformancePackId "Use generic-lens or generic-optics with 'conformancePackId' instead"  #-}

-- | Amazon Resource Name (ARN) of comformance pack.
--
-- /Note:/ Consider using 'conformancePackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackArn :: Lens.Lens' ConformancePackStatusDetail Types.ConformancePackArn
cpsdConformancePackArn = Lens.field @"conformancePackArn"
{-# INLINEABLE cpsdConformancePackArn #-}
{-# DEPRECATED conformancePackArn "Use generic-lens or generic-optics with 'conformancePackArn' instead"  #-}

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
cpsdConformancePackState :: Lens.Lens' ConformancePackStatusDetail Types.ConformancePackState
cpsdConformancePackState = Lens.field @"conformancePackState"
{-# INLINEABLE cpsdConformancePackState #-}
{-# DEPRECATED conformancePackState "Use generic-lens or generic-optics with 'conformancePackState' instead"  #-}

-- | Amazon Resource Name (ARN) of AWS CloudFormation stack. 
--
-- /Note:/ Consider using 'stackArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdStackArn :: Lens.Lens' ConformancePackStatusDetail Types.StackArn
cpsdStackArn = Lens.field @"stackArn"
{-# INLINEABLE cpsdStackArn #-}
{-# DEPRECATED stackArn "Use generic-lens or generic-optics with 'stackArn' instead"  #-}

-- | Last time when conformation pack creation and update was requested.
--
-- /Note:/ Consider using 'lastUpdateRequestedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdLastUpdateRequestedTime :: Lens.Lens' ConformancePackStatusDetail Core.NominalDiffTime
cpsdLastUpdateRequestedTime = Lens.field @"lastUpdateRequestedTime"
{-# INLINEABLE cpsdLastUpdateRequestedTime #-}
{-# DEPRECATED lastUpdateRequestedTime "Use generic-lens or generic-optics with 'lastUpdateRequestedTime' instead"  #-}

-- | The reason of conformance pack creation failure.
--
-- /Note:/ Consider using 'conformancePackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdConformancePackStatusReason :: Lens.Lens' ConformancePackStatusDetail (Core.Maybe Types.ConformancePackStatusReason)
cpsdConformancePackStatusReason = Lens.field @"conformancePackStatusReason"
{-# INLINEABLE cpsdConformancePackStatusReason #-}
{-# DEPRECATED conformancePackStatusReason "Use generic-lens or generic-optics with 'conformancePackStatusReason' instead"  #-}

-- | Last time when conformation pack creation and update was successful.
--
-- /Note:/ Consider using 'lastUpdateCompletedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsdLastUpdateCompletedTime :: Lens.Lens' ConformancePackStatusDetail (Core.Maybe Core.NominalDiffTime)
cpsdLastUpdateCompletedTime = Lens.field @"lastUpdateCompletedTime"
{-# INLINEABLE cpsdLastUpdateCompletedTime #-}
{-# DEPRECATED lastUpdateCompletedTime "Use generic-lens or generic-optics with 'lastUpdateCompletedTime' instead"  #-}

instance Core.FromJSON ConformancePackStatusDetail where
        parseJSON
          = Core.withObject "ConformancePackStatusDetail" Core.$
              \ x ->
                ConformancePackStatusDetail' Core.<$>
                  (x Core..: "ConformancePackName") Core.<*>
                    x Core..: "ConformancePackId"
                    Core.<*> x Core..: "ConformancePackArn"
                    Core.<*> x Core..: "ConformancePackState"
                    Core.<*> x Core..: "StackArn"
                    Core.<*> x Core..: "LastUpdateRequestedTime"
                    Core.<*> x Core..:? "ConformancePackStatusReason"
                    Core.<*> x Core..:? "LastUpdateCompletedTime"
