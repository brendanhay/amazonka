{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.DelegatedAdministrator
  ( DelegatedAdministrator (..)
  -- * Smart constructor
  , mkDelegatedAdministrator
  -- * Lenses
  , daArn
  , daDelegationEnabledDate
  , daEmail
  , daId
  , daJoinedMethod
  , daJoinedTimestamp
  , daName
  , daStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.AccountArn as Types
import qualified Network.AWS.Organizations.Types.AccountId as Types
import qualified Network.AWS.Organizations.Types.AccountJoinedMethod as Types
import qualified Network.AWS.Organizations.Types.AccountName as Types
import qualified Network.AWS.Organizations.Types.AccountStatus as Types
import qualified Network.AWS.Organizations.Types.Email as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about the delegated administrator.
--
-- /See:/ 'mkDelegatedAdministrator' smart constructor.
data DelegatedAdministrator = DelegatedAdministrator'
  { arn :: Core.Maybe Types.AccountArn
    -- ^ The Amazon Resource Name (ARN) of the delegated administrator's account.
  , delegationEnabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the account was made a delegated administrator.
  , email :: Core.Maybe Types.Email
    -- ^ The email address that is associated with the delegated administrator's AWS account.
  , id :: Core.Maybe Types.AccountId
    -- ^ The unique identifier (ID) of the delegated administrator's account.
  , joinedMethod :: Core.Maybe Types.AccountJoinedMethod
    -- ^ The method by which the delegated administrator's account joined the organization.
  , joinedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the delegated administrator's account became a part of the organization.
  , name :: Core.Maybe Types.AccountName
    -- ^ The friendly name of the delegated administrator's account.
  , status :: Core.Maybe Types.AccountStatus
    -- ^ The status of the delegated administrator's account in the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DelegatedAdministrator' value with any optional fields omitted.
mkDelegatedAdministrator
    :: DelegatedAdministrator
mkDelegatedAdministrator
  = DelegatedAdministrator'{arn = Core.Nothing,
                            delegationEnabledDate = Core.Nothing, email = Core.Nothing,
                            id = Core.Nothing, joinedMethod = Core.Nothing,
                            joinedTimestamp = Core.Nothing, name = Core.Nothing,
                            status = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the delegated administrator's account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daArn :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountArn)
daArn = Lens.field @"arn"
{-# INLINEABLE daArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date when the account was made a delegated administrator.
--
-- /Note:/ Consider using 'delegationEnabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDelegationEnabledDate :: Lens.Lens' DelegatedAdministrator (Core.Maybe Core.NominalDiffTime)
daDelegationEnabledDate = Lens.field @"delegationEnabledDate"
{-# INLINEABLE daDelegationEnabledDate #-}
{-# DEPRECATED delegationEnabledDate "Use generic-lens or generic-optics with 'delegationEnabledDate' instead"  #-}

-- | The email address that is associated with the delegated administrator's AWS account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEmail :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.Email)
daEmail = Lens.field @"email"
{-# INLINEABLE daEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The unique identifier (ID) of the delegated administrator's account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daId :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountId)
daId = Lens.field @"id"
{-# INLINEABLE daId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The method by which the delegated administrator's account joined the organization.
--
-- /Note:/ Consider using 'joinedMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daJoinedMethod :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountJoinedMethod)
daJoinedMethod = Lens.field @"joinedMethod"
{-# INLINEABLE daJoinedMethod #-}
{-# DEPRECATED joinedMethod "Use generic-lens or generic-optics with 'joinedMethod' instead"  #-}

-- | The date when the delegated administrator's account became a part of the organization.
--
-- /Note:/ Consider using 'joinedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daJoinedTimestamp :: Lens.Lens' DelegatedAdministrator (Core.Maybe Core.NominalDiffTime)
daJoinedTimestamp = Lens.field @"joinedTimestamp"
{-# INLINEABLE daJoinedTimestamp #-}
{-# DEPRECATED joinedTimestamp "Use generic-lens or generic-optics with 'joinedTimestamp' instead"  #-}

-- | The friendly name of the delegated administrator's account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountName)
daName = Lens.field @"name"
{-# INLINEABLE daName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of the delegated administrator's account in the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStatus :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountStatus)
daStatus = Lens.field @"status"
{-# INLINEABLE daStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON DelegatedAdministrator where
        parseJSON
          = Core.withObject "DelegatedAdministrator" Core.$
              \ x ->
                DelegatedAdministrator' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "DelegationEnabledDate"
                    Core.<*> x Core..:? "Email"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "JoinedMethod"
                    Core.<*> x Core..:? "JoinedTimestamp"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Status"
