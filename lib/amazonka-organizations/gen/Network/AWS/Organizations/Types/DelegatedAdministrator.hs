{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.DelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedAdministrator
  ( DelegatedAdministrator (..),

    -- * Smart constructor
    mkDelegatedAdministrator,

    -- * Lenses
    daArn,
    daDelegationEnabledDate,
    daEmail,
    daId,
    daJoinedMethod,
    daJoinedTimestamp,
    daName,
    daStatus,
  )
where

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
  { -- | The Amazon Resource Name (ARN) of the delegated administrator's account.
    arn :: Core.Maybe Types.AccountArn,
    -- | The date when the account was made a delegated administrator.
    delegationEnabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The email address that is associated with the delegated administrator's AWS account.
    email :: Core.Maybe Types.Email,
    -- | The unique identifier (ID) of the delegated administrator's account.
    id :: Core.Maybe Types.AccountId,
    -- | The method by which the delegated administrator's account joined the organization.
    joinedMethod :: Core.Maybe Types.AccountJoinedMethod,
    -- | The date when the delegated administrator's account became a part of the organization.
    joinedTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The friendly name of the delegated administrator's account.
    name :: Core.Maybe Types.AccountName,
    -- | The status of the delegated administrator's account in the organization.
    status :: Core.Maybe Types.AccountStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DelegatedAdministrator' value with any optional fields omitted.
mkDelegatedAdministrator ::
  DelegatedAdministrator
mkDelegatedAdministrator =
  DelegatedAdministrator'
    { arn = Core.Nothing,
      delegationEnabledDate = Core.Nothing,
      email = Core.Nothing,
      id = Core.Nothing,
      joinedMethod = Core.Nothing,
      joinedTimestamp = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the delegated administrator's account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daArn :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountArn)
daArn = Lens.field @"arn"
{-# DEPRECATED daArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the account was made a delegated administrator.
--
-- /Note:/ Consider using 'delegationEnabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDelegationEnabledDate :: Lens.Lens' DelegatedAdministrator (Core.Maybe Core.NominalDiffTime)
daDelegationEnabledDate = Lens.field @"delegationEnabledDate"
{-# DEPRECATED daDelegationEnabledDate "Use generic-lens or generic-optics with 'delegationEnabledDate' instead." #-}

-- | The email address that is associated with the delegated administrator's AWS account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEmail :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.Email)
daEmail = Lens.field @"email"
{-# DEPRECATED daEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The unique identifier (ID) of the delegated administrator's account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daId :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountId)
daId = Lens.field @"id"
{-# DEPRECATED daId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The method by which the delegated administrator's account joined the organization.
--
-- /Note:/ Consider using 'joinedMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daJoinedMethod :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountJoinedMethod)
daJoinedMethod = Lens.field @"joinedMethod"
{-# DEPRECATED daJoinedMethod "Use generic-lens or generic-optics with 'joinedMethod' instead." #-}

-- | The date when the delegated administrator's account became a part of the organization.
--
-- /Note:/ Consider using 'joinedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daJoinedTimestamp :: Lens.Lens' DelegatedAdministrator (Core.Maybe Core.NominalDiffTime)
daJoinedTimestamp = Lens.field @"joinedTimestamp"
{-# DEPRECATED daJoinedTimestamp "Use generic-lens or generic-optics with 'joinedTimestamp' instead." #-}

-- | The friendly name of the delegated administrator's account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daName :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountName)
daName = Lens.field @"name"
{-# DEPRECATED daName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status of the delegated administrator's account in the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStatus :: Lens.Lens' DelegatedAdministrator (Core.Maybe Types.AccountStatus)
daStatus = Lens.field @"status"
{-# DEPRECATED daStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON DelegatedAdministrator where
  parseJSON =
    Core.withObject "DelegatedAdministrator" Core.$
      \x ->
        DelegatedAdministrator'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "DelegationEnabledDate")
          Core.<*> (x Core..:? "Email")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "JoinedMethod")
          Core.<*> (x Core..:? "JoinedTimestamp")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Status")
