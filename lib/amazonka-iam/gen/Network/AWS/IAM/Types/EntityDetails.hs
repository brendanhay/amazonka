{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityDetails
  ( EntityDetails (..),

    -- * Smart constructor
    mkEntityDetails,

    -- * Lenses
    edEntityInfo,
    edLastAuthenticated,
  )
where

import qualified Network.AWS.IAM.Types.EntityInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains details about when the IAM entities (users or roles) were last used in an attempt to access the specified AWS service.
--
-- This data type is a response element in the 'GetServiceLastAccessedDetailsWithEntities' operation.
--
-- /See:/ 'mkEntityDetails' smart constructor.
data EntityDetails = EntityDetails'
  { -- | The @EntityInfo@ object that contains details about the entity (user or role).
    entityInfo :: Types.EntityInfo,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the authenticated entity last attempted to access AWS. AWS does not report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
    lastAuthenticated :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EntityDetails' value with any optional fields omitted.
mkEntityDetails ::
  -- | 'entityInfo'
  Types.EntityInfo ->
  EntityDetails
mkEntityDetails entityInfo =
  EntityDetails' {entityInfo, lastAuthenticated = Core.Nothing}

-- | The @EntityInfo@ object that contains details about the entity (user or role).
--
-- /Note:/ Consider using 'entityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEntityInfo :: Lens.Lens' EntityDetails Types.EntityInfo
edEntityInfo = Lens.field @"entityInfo"
{-# DEPRECATED edEntityInfo "Use generic-lens or generic-optics with 'entityInfo' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the authenticated entity last attempted to access AWS. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edLastAuthenticated :: Lens.Lens' EntityDetails (Core.Maybe Core.UTCTime)
edLastAuthenticated = Lens.field @"lastAuthenticated"
{-# DEPRECATED edLastAuthenticated "Use generic-lens or generic-optics with 'lastAuthenticated' instead." #-}

instance Core.FromXML EntityDetails where
  parseXML x =
    EntityDetails'
      Core.<$> (x Core..@ "EntityInfo") Core.<*> (x Core..@? "LastAuthenticated")
