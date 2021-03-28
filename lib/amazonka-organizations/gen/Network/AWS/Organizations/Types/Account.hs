{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Account
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.Account
  ( Account (..)
  -- * Smart constructor
  , mkAccount
  -- * Lenses
  , aArn
  , aEmail
  , aId
  , aJoinedMethod
  , aJoinedTimestamp
  , aName
  , aStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.AccountJoinedMethod as Types
import qualified Network.AWS.Organizations.Types.AccountName as Types
import qualified Network.AWS.Organizations.Types.AccountStatus as Types
import qualified Network.AWS.Organizations.Types.Arn as Types
import qualified Network.AWS.Organizations.Types.Email as Types
import qualified Network.AWS.Organizations.Types.Id as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about an AWS account that is a member of an organization.
--
-- /See:/ 'mkAccount' smart constructor.
data Account = Account'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the account.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
  , email :: Core.Maybe Types.Email
    -- ^ The email address associated with the AWS account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters that represents a standard internet email address.
  , id :: Core.Maybe Types.Id
    -- ^ The unique identifier (ID) of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
  , joinedMethod :: Core.Maybe Types.AccountJoinedMethod
    -- ^ The method by which the account joined the organization.
  , joinedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the account became a part of the organization.
  , name :: Core.Maybe Types.AccountName
    -- ^ The friendly name of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
  , status :: Core.Maybe Types.AccountStatus
    -- ^ The status of the account in the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Account' value with any optional fields omitted.
mkAccount
    :: Account
mkAccount
  = Account'{arn = Core.Nothing, email = Core.Nothing,
             id = Core.Nothing, joinedMethod = Core.Nothing,
             joinedTimestamp = Core.Nothing, name = Core.Nothing,
             status = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the account.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArn :: Lens.Lens' Account (Core.Maybe Types.Arn)
aArn = Lens.field @"arn"
{-# INLINEABLE aArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The email address associated with the AWS account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters that represents a standard internet email address.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEmail :: Lens.Lens' Account (Core.Maybe Types.Email)
aEmail = Lens.field @"email"
{-# INLINEABLE aEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The unique identifier (ID) of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' Account (Core.Maybe Types.Id)
aId = Lens.field @"id"
{-# INLINEABLE aId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The method by which the account joined the organization.
--
-- /Note:/ Consider using 'joinedMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aJoinedMethod :: Lens.Lens' Account (Core.Maybe Types.AccountJoinedMethod)
aJoinedMethod = Lens.field @"joinedMethod"
{-# INLINEABLE aJoinedMethod #-}
{-# DEPRECATED joinedMethod "Use generic-lens or generic-optics with 'joinedMethod' instead"  #-}

-- | The date the account became a part of the organization.
--
-- /Note:/ Consider using 'joinedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aJoinedTimestamp :: Lens.Lens' Account (Core.Maybe Core.NominalDiffTime)
aJoinedTimestamp = Lens.field @"joinedTimestamp"
{-# INLINEABLE aJoinedTimestamp #-}
{-# DEPRECATED joinedTimestamp "Use generic-lens or generic-optics with 'joinedTimestamp' instead"  #-}

-- | The friendly name of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Account (Core.Maybe Types.AccountName)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of the account in the organization.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatus :: Lens.Lens' Account (Core.Maybe Types.AccountStatus)
aStatus = Lens.field @"status"
{-# INLINEABLE aStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON Account where
        parseJSON
          = Core.withObject "Account" Core.$
              \ x ->
                Account' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Email" Core.<*>
                    x Core..:? "Id"
                    Core.<*> x Core..:? "JoinedMethod"
                    Core.<*> x Core..:? "JoinedTimestamp"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Status"
