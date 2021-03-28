{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.GrantListEntry
  ( GrantListEntry (..)
  -- * Smart constructor
  , mkGrantListEntry
  -- * Lenses
  , gleConstraints
  , gleCreationDate
  , gleGrantId
  , gleGranteePrincipal
  , gleIssuingAccount
  , gleKeyId
  , gleName
  , gleOperations
  , gleRetiringPrincipal
  ) where

import qualified Network.AWS.KMS.Types.GrantConstraints as Types
import qualified Network.AWS.KMS.Types.GrantId as Types
import qualified Network.AWS.KMS.Types.GrantOperation as Types
import qualified Network.AWS.KMS.Types.GranteePrincipal as Types
import qualified Network.AWS.KMS.Types.IssuingAccount as Types
import qualified Network.AWS.KMS.Types.KeyId as Types
import qualified Network.AWS.KMS.Types.Name as Types
import qualified Network.AWS.KMS.Types.RetiringPrincipal as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a grant.
--
-- /See:/ 'mkGrantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
  { constraints :: Core.Maybe Types.GrantConstraints
    -- ^ A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the grant was created.
  , grantId :: Core.Maybe Types.GrantId
    -- ^ The unique identifier for the grant.
  , granteePrincipal :: Core.Maybe Types.GranteePrincipal
    -- ^ The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually contains the user or role designated as the grantee principal in the grant. However, when the grantee principal in the grant is an AWS service, the @GranteePrincipal@ field contains the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal> , which might represent several different grantee principals.
  , issuingAccount :: Core.Maybe Types.IssuingAccount
    -- ^ The AWS account under which the grant was issued.
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The unique identifier for the customer master key (CMK) to which the grant applies.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
  , operations :: Core.Maybe [Types.GrantOperation]
    -- ^ The list of operations permitted by the grant.
  , retiringPrincipal :: Core.Maybe Types.RetiringPrincipal
    -- ^ The principal that can retire the grant.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GrantListEntry' value with any optional fields omitted.
mkGrantListEntry
    :: GrantListEntry
mkGrantListEntry
  = GrantListEntry'{constraints = Core.Nothing,
                    creationDate = Core.Nothing, grantId = Core.Nothing,
                    granteePrincipal = Core.Nothing, issuingAccount = Core.Nothing,
                    keyId = Core.Nothing, name = Core.Nothing,
                    operations = Core.Nothing, retiringPrincipal = Core.Nothing}

-- | A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleConstraints :: Lens.Lens' GrantListEntry (Core.Maybe Types.GrantConstraints)
gleConstraints = Lens.field @"constraints"
{-# INLINEABLE gleConstraints #-}
{-# DEPRECATED constraints "Use generic-lens or generic-optics with 'constraints' instead"  #-}

-- | The date and time when the grant was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleCreationDate :: Lens.Lens' GrantListEntry (Core.Maybe Core.NominalDiffTime)
gleCreationDate = Lens.field @"creationDate"
{-# INLINEABLE gleCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The unique identifier for the grant.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleGrantId :: Lens.Lens' GrantListEntry (Core.Maybe Types.GrantId)
gleGrantId = Lens.field @"grantId"
{-# INLINEABLE gleGrantId #-}
{-# DEPRECATED grantId "Use generic-lens or generic-optics with 'grantId' instead"  #-}

-- | The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually contains the user or role designated as the grantee principal in the grant. However, when the grantee principal in the grant is an AWS service, the @GranteePrincipal@ field contains the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal> , which might represent several different grantee principals.
--
-- /Note:/ Consider using 'granteePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleGranteePrincipal :: Lens.Lens' GrantListEntry (Core.Maybe Types.GranteePrincipal)
gleGranteePrincipal = Lens.field @"granteePrincipal"
{-# INLINEABLE gleGranteePrincipal #-}
{-# DEPRECATED granteePrincipal "Use generic-lens or generic-optics with 'granteePrincipal' instead"  #-}

-- | The AWS account under which the grant was issued.
--
-- /Note:/ Consider using 'issuingAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleIssuingAccount :: Lens.Lens' GrantListEntry (Core.Maybe Types.IssuingAccount)
gleIssuingAccount = Lens.field @"issuingAccount"
{-# INLINEABLE gleIssuingAccount #-}
{-# DEPRECATED issuingAccount "Use generic-lens or generic-optics with 'issuingAccount' instead"  #-}

-- | The unique identifier for the customer master key (CMK) to which the grant applies.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleKeyId :: Lens.Lens' GrantListEntry (Core.Maybe Types.KeyId)
gleKeyId = Lens.field @"keyId"
{-# INLINEABLE gleKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleName :: Lens.Lens' GrantListEntry (Core.Maybe Types.Name)
gleName = Lens.field @"name"
{-# INLINEABLE gleName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The list of operations permitted by the grant.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleOperations :: Lens.Lens' GrantListEntry (Core.Maybe [Types.GrantOperation])
gleOperations = Lens.field @"operations"
{-# INLINEABLE gleOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The principal that can retire the grant.
--
-- /Note:/ Consider using 'retiringPrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleRetiringPrincipal :: Lens.Lens' GrantListEntry (Core.Maybe Types.RetiringPrincipal)
gleRetiringPrincipal = Lens.field @"retiringPrincipal"
{-# INLINEABLE gleRetiringPrincipal #-}
{-# DEPRECATED retiringPrincipal "Use generic-lens or generic-optics with 'retiringPrincipal' instead"  #-}

instance Core.FromJSON GrantListEntry where
        parseJSON
          = Core.withObject "GrantListEntry" Core.$
              \ x ->
                GrantListEntry' Core.<$>
                  (x Core..:? "Constraints") Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "GrantId"
                    Core.<*> x Core..:? "GranteePrincipal"
                    Core.<*> x Core..:? "IssuingAccount"
                    Core.<*> x Core..:? "KeyId"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Operations"
                    Core.<*> x Core..:? "RetiringPrincipal"
