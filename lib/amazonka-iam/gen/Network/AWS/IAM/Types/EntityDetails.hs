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
    edLastAuthenticated,
    edEntityInfo,
  )
where

import Network.AWS.IAM.Types.EntityInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains details about when the IAM entities (users or roles) were last used in an attempt to access the specified AWS service.
--
-- This data type is a response element in the 'GetServiceLastAccessedDetailsWithEntities' operation.
--
-- /See:/ 'mkEntityDetails' smart constructor.
data EntityDetails = EntityDetails'
  { lastAuthenticated ::
      Lude.Maybe Lude.ISO8601,
    entityInfo :: EntityInfo
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityDetails' with the minimum fields required to make a request.
--
-- * 'entityInfo' - The @EntityInfo@ object that contains details about the entity (user or role).
-- * 'lastAuthenticated' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the authenticated entity last attempted to access AWS. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
mkEntityDetails ::
  -- | 'entityInfo'
  EntityInfo ->
  EntityDetails
mkEntityDetails pEntityInfo_ =
  EntityDetails'
    { lastAuthenticated = Lude.Nothing,
      entityInfo = pEntityInfo_
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the authenticated entity last attempted to access AWS. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edLastAuthenticated :: Lens.Lens' EntityDetails (Lude.Maybe Lude.ISO8601)
edLastAuthenticated = Lens.lens (lastAuthenticated :: EntityDetails -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastAuthenticated = a} :: EntityDetails)
{-# DEPRECATED edLastAuthenticated "Use generic-lens or generic-optics with 'lastAuthenticated' instead." #-}

-- | The @EntityInfo@ object that contains details about the entity (user or role).
--
-- /Note:/ Consider using 'entityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEntityInfo :: Lens.Lens' EntityDetails EntityInfo
edEntityInfo = Lens.lens (entityInfo :: EntityDetails -> EntityInfo) (\s a -> s {entityInfo = a} :: EntityDetails)
{-# DEPRECATED edEntityInfo "Use generic-lens or generic-optics with 'entityInfo' instead." #-}

instance Lude.FromXML EntityDetails where
  parseXML x =
    EntityDetails'
      Lude.<$> (x Lude..@? "LastAuthenticated") Lude.<*> (x Lude..@ "EntityInfo")
