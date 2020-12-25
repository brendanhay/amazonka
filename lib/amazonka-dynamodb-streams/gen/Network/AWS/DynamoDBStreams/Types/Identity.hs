{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Identity
  ( Identity (..),

    -- * Smart constructor
    mkIdentity,

    -- * Lenses
    iPrincipalId,
    iType,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the type of identity that made the request.
--
-- /See:/ 'mkIdentity' smart constructor.
data Identity = Identity'
  { -- | A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
    principalId :: Core.Maybe Types.String,
    -- | The type of the identity. For Time To Live, the type is "Service".
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Identity' value with any optional fields omitted.
mkIdentity ::
  Identity
mkIdentity =
  Identity' {principalId = Core.Nothing, type' = Core.Nothing}

-- | A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrincipalId :: Lens.Lens' Identity (Core.Maybe Types.String)
iPrincipalId = Lens.field @"principalId"
{-# DEPRECATED iPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The type of the identity. For Time To Live, the type is "Service".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Identity (Core.Maybe Types.String)
iType = Lens.field @"type'"
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Identity where
  parseJSON =
    Core.withObject "Identity" Core.$
      \x ->
        Identity'
          Core.<$> (x Core..:? "PrincipalId") Core.<*> (x Core..:? "Type")
