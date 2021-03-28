{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Initiator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Initiator
  ( Initiator (..)
  -- * Smart constructor
  , mkInitiator
  -- * Lenses
  , iDisplayName
  , iID
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.DisplayName as Types
import qualified Network.AWS.S3.Types.ID as Types

-- | Container element that identifies who initiated the multipart upload. 
--
-- /See:/ 'mkInitiator' smart constructor.
data Initiator = Initiator'
  { displayName :: Core.Maybe Types.DisplayName
    -- ^ Name of the Principal.
  , id :: Core.Maybe Types.ID
    -- ^ If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Initiator' value with any optional fields omitted.
mkInitiator
    :: Initiator
mkInitiator
  = Initiator'{displayName = Core.Nothing, id = Core.Nothing}

-- | Name of the Principal.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDisplayName :: Lens.Lens' Initiator (Core.Maybe Types.DisplayName)
iDisplayName = Lens.field @"displayName"
{-# INLINEABLE iDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iID :: Lens.Lens' Initiator (Core.Maybe Types.ID)
iID = Lens.field @"id"
{-# INLINEABLE iID #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromXML Initiator where
        parseXML x
          = Initiator' Core.<$>
              (x Core..@? "DisplayName") Core.<*> x Core..@? "ID"
