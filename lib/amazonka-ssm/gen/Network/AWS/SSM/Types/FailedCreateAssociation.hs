{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.FailedCreateAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.FailedCreateAssociation
  ( FailedCreateAssociation (..)
  -- * Smart constructor
  , mkFailedCreateAssociation
  -- * Lenses
  , fcaEntry
  , fcaFault
  , fcaMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.BatchErrorMessage as Types
import qualified Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry as Types
import qualified Network.AWS.SSM.Types.Fault as Types

-- | Describes a failed association.
--
-- /See:/ 'mkFailedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { entry :: Core.Maybe Types.CreateAssociationBatchRequestEntry
    -- ^ The association.
  , fault :: Core.Maybe Types.Fault
    -- ^ The source of the failure.
  , message :: Core.Maybe Types.BatchErrorMessage
    -- ^ A description of the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedCreateAssociation' value with any optional fields omitted.
mkFailedCreateAssociation
    :: FailedCreateAssociation
mkFailedCreateAssociation
  = FailedCreateAssociation'{entry = Core.Nothing,
                             fault = Core.Nothing, message = Core.Nothing}

-- | The association.
--
-- /Note:/ Consider using 'entry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcaEntry :: Lens.Lens' FailedCreateAssociation (Core.Maybe Types.CreateAssociationBatchRequestEntry)
fcaEntry = Lens.field @"entry"
{-# INLINEABLE fcaEntry #-}
{-# DEPRECATED entry "Use generic-lens or generic-optics with 'entry' instead"  #-}

-- | The source of the failure.
--
-- /Note:/ Consider using 'fault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcaFault :: Lens.Lens' FailedCreateAssociation (Core.Maybe Types.Fault)
fcaFault = Lens.field @"fault"
{-# INLINEABLE fcaFault #-}
{-# DEPRECATED fault "Use generic-lens or generic-optics with 'fault' instead"  #-}

-- | A description of the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcaMessage :: Lens.Lens' FailedCreateAssociation (Core.Maybe Types.BatchErrorMessage)
fcaMessage = Lens.field @"message"
{-# INLINEABLE fcaMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON FailedCreateAssociation where
        parseJSON
          = Core.withObject "FailedCreateAssociation" Core.$
              \ x ->
                FailedCreateAssociation' Core.<$>
                  (x Core..:? "Entry") Core.<*> x Core..:? "Fault" Core.<*>
                    x Core..:? "Message"
