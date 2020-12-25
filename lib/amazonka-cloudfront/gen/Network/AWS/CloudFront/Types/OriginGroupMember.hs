{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMember
  ( OriginGroupMember (..),

    -- * Smart constructor
    mkOriginGroupMember,

    -- * Lenses
    ogmOriginId,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An origin in an origin group.
--
-- /See:/ 'mkOriginGroupMember' smart constructor.
newtype OriginGroupMember = OriginGroupMember'
  { -- | The ID for an origin in an origin group.
    originId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OriginGroupMember' value with any optional fields omitted.
mkOriginGroupMember ::
  -- | 'originId'
  Types.String ->
  OriginGroupMember
mkOriginGroupMember originId = OriginGroupMember' {originId}

-- | The ID for an origin in an origin group.
--
-- /Note:/ Consider using 'originId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogmOriginId :: Lens.Lens' OriginGroupMember Types.String
ogmOriginId = Lens.field @"originId"
{-# DEPRECATED ogmOriginId "Use generic-lens or generic-optics with 'originId' instead." #-}

instance Core.ToXML OriginGroupMember where
  toXML OriginGroupMember {..} = Core.toXMLNode "OriginId" originId

instance Core.FromXML OriginGroupMember where
  parseXML x = OriginGroupMember' Core.<$> (x Core..@ "OriginId")
