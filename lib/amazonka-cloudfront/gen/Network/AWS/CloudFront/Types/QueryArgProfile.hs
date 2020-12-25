{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfile
  ( QueryArgProfile (..),

    -- * Smart constructor
    mkQueryArgProfile,

    -- * Lenses
    qapQueryArg,
    qapProfileId,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Query argument-profile mapping for field-level encryption.
--
-- /See:/ 'mkQueryArgProfile' smart constructor.
data QueryArgProfile = QueryArgProfile'
  { -- | Query argument for field-level encryption query argument-profile mapping.
    queryArg :: Types.String,
    -- | ID of profile to use for field-level encryption query argument-profile mapping
    profileId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryArgProfile' value with any optional fields omitted.
mkQueryArgProfile ::
  -- | 'queryArg'
  Types.String ->
  -- | 'profileId'
  Types.String ->
  QueryArgProfile
mkQueryArgProfile queryArg profileId =
  QueryArgProfile' {queryArg, profileId}

-- | Query argument for field-level encryption query argument-profile mapping.
--
-- /Note:/ Consider using 'queryArg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapQueryArg :: Lens.Lens' QueryArgProfile Types.String
qapQueryArg = Lens.field @"queryArg"
{-# DEPRECATED qapQueryArg "Use generic-lens or generic-optics with 'queryArg' instead." #-}

-- | ID of profile to use for field-level encryption query argument-profile mapping
--
-- /Note:/ Consider using 'profileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qapProfileId :: Lens.Lens' QueryArgProfile Types.String
qapProfileId = Lens.field @"profileId"
{-# DEPRECATED qapProfileId "Use generic-lens or generic-optics with 'profileId' instead." #-}

instance Core.ToXML QueryArgProfile where
  toXML QueryArgProfile {..} =
    Core.toXMLNode "QueryArg" queryArg
      Core.<> Core.toXMLNode "ProfileId" profileId

instance Core.FromXML QueryArgProfile where
  parseXML x =
    QueryArgProfile'
      Core.<$> (x Core..@ "QueryArg") Core.<*> (x Core..@ "ProfileId")
