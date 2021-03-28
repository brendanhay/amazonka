{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.MitigationActionIdentifier
  ( MitigationActionIdentifier (..)
  -- * Smart constructor
  , mkMitigationActionIdentifier
  -- * Lenses
  , maiActionArn
  , maiActionName
  , maiCreationDate
  ) where

import qualified Network.AWS.IoT.Types.MitigationActionArn as Types
import qualified Network.AWS.IoT.Types.MitigationActionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information that identifies a mitigation action. This information is returned by ListMitigationActions.
--
-- /See:/ 'mkMitigationActionIdentifier' smart constructor.
data MitigationActionIdentifier = MitigationActionIdentifier'
  { actionArn :: Core.Maybe Types.MitigationActionArn
    -- ^ The IAM role ARN used to apply this mitigation action.
  , actionName :: Core.Maybe Types.MitigationActionName
    -- ^ The friendly name of the mitigation action.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when this mitigation action was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MitigationActionIdentifier' value with any optional fields omitted.
mkMitigationActionIdentifier
    :: MitigationActionIdentifier
mkMitigationActionIdentifier
  = MitigationActionIdentifier'{actionArn = Core.Nothing,
                                actionName = Core.Nothing, creationDate = Core.Nothing}

-- | The IAM role ARN used to apply this mitigation action.
--
-- /Note:/ Consider using 'actionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maiActionArn :: Lens.Lens' MitigationActionIdentifier (Core.Maybe Types.MitigationActionArn)
maiActionArn = Lens.field @"actionArn"
{-# INLINEABLE maiActionArn #-}
{-# DEPRECATED actionArn "Use generic-lens or generic-optics with 'actionArn' instead"  #-}

-- | The friendly name of the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maiActionName :: Lens.Lens' MitigationActionIdentifier (Core.Maybe Types.MitigationActionName)
maiActionName = Lens.field @"actionName"
{-# INLINEABLE maiActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | The date when this mitigation action was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maiCreationDate :: Lens.Lens' MitigationActionIdentifier (Core.Maybe Core.NominalDiffTime)
maiCreationDate = Lens.field @"creationDate"
{-# INLINEABLE maiCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

instance Core.FromJSON MitigationActionIdentifier where
        parseJSON
          = Core.withObject "MitigationActionIdentifier" Core.$
              \ x ->
                MitigationActionIdentifier' Core.<$>
                  (x Core..:? "actionArn") Core.<*> x Core..:? "actionName" Core.<*>
                    x Core..:? "creationDate"
