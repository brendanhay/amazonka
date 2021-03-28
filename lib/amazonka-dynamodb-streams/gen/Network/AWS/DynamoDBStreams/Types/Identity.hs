{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.Identity
  ( Identity (..)
  -- * Smart constructor
  , mkIdentity
  -- * Lenses
  , iPrincipalId
  , iType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the type of identity that made the request.
--
-- /See:/ 'mkIdentity' smart constructor.
data Identity = Identity'
  { principalId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
  , type' :: Core.Maybe Core.Text
    -- ^ The type of the identity. For Time To Live, the type is "Service".
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Identity' value with any optional fields omitted.
mkIdentity
    :: Identity
mkIdentity
  = Identity'{principalId = Core.Nothing, type' = Core.Nothing}

-- | A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrincipalId :: Lens.Lens' Identity (Core.Maybe Core.Text)
iPrincipalId = Lens.field @"principalId"
{-# INLINEABLE iPrincipalId #-}
{-# DEPRECATED principalId "Use generic-lens or generic-optics with 'principalId' instead"  #-}

-- | The type of the identity. For Time To Live, the type is "Service".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Identity (Core.Maybe Core.Text)
iType = Lens.field @"type'"
{-# INLINEABLE iType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Identity where
        parseJSON
          = Core.withObject "Identity" Core.$
              \ x ->
                Identity' Core.<$>
                  (x Core..:? "PrincipalId") Core.<*> x Core..:? "Type"
