-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantListEntry
  ( GrantListEntry (..),

    -- * Smart constructor
    mkGrantListEntry,

    -- * Lenses
    gleKeyId,
    gleRetiringPrincipal,
    gleIssuingAccount,
    gleGrantId,
    gleConstraints,
    gleGranteePrincipal,
    gleName,
    gleCreationDate,
    gleOperations,
  )
where

import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantOperation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a grant.
--
-- /See:/ 'mkGrantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
  { keyId ::
      Lude.Maybe Lude.Text,
    retiringPrincipal :: Lude.Maybe Lude.Text,
    issuingAccount :: Lude.Maybe Lude.Text,
    grantId :: Lude.Maybe Lude.Text,
    constraints :: Lude.Maybe GrantConstraints,
    granteePrincipal :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    operations :: Lude.Maybe [GrantOperation]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GrantListEntry' with the minimum fields required to make a request.
--
-- * 'constraints' - A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
-- * 'creationDate' - The date and time when the grant was created.
-- * 'grantId' - The unique identifier for the grant.
-- * 'granteePrincipal' - The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually contains the user or role designated as the grantee principal in the grant. However, when the grantee principal in the grant is an AWS service, the @GranteePrincipal@ field contains the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal> , which might represent several different grantee principals.
-- * 'issuingAccount' - The AWS account under which the grant was issued.
-- * 'keyId' - The unique identifier for the customer master key (CMK) to which the grant applies.
-- * 'name' - The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
-- * 'operations' - The list of operations permitted by the grant.
-- * 'retiringPrincipal' - The principal that can retire the grant.
mkGrantListEntry ::
  GrantListEntry
mkGrantListEntry =
  GrantListEntry'
    { keyId = Lude.Nothing,
      retiringPrincipal = Lude.Nothing,
      issuingAccount = Lude.Nothing,
      grantId = Lude.Nothing,
      constraints = Lude.Nothing,
      granteePrincipal = Lude.Nothing,
      name = Lude.Nothing,
      creationDate = Lude.Nothing,
      operations = Lude.Nothing
    }

-- | The unique identifier for the customer master key (CMK) to which the grant applies.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleKeyId :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Text)
gleKeyId = Lens.lens (keyId :: GrantListEntry -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GrantListEntry)
{-# DEPRECATED gleKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The principal that can retire the grant.
--
-- /Note:/ Consider using 'retiringPrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleRetiringPrincipal :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Text)
gleRetiringPrincipal = Lens.lens (retiringPrincipal :: GrantListEntry -> Lude.Maybe Lude.Text) (\s a -> s {retiringPrincipal = a} :: GrantListEntry)
{-# DEPRECATED gleRetiringPrincipal "Use generic-lens or generic-optics with 'retiringPrincipal' instead." #-}

-- | The AWS account under which the grant was issued.
--
-- /Note:/ Consider using 'issuingAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleIssuingAccount :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Text)
gleIssuingAccount = Lens.lens (issuingAccount :: GrantListEntry -> Lude.Maybe Lude.Text) (\s a -> s {issuingAccount = a} :: GrantListEntry)
{-# DEPRECATED gleIssuingAccount "Use generic-lens or generic-optics with 'issuingAccount' instead." #-}

-- | The unique identifier for the grant.
--
-- /Note:/ Consider using 'grantId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleGrantId :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Text)
gleGrantId = Lens.lens (grantId :: GrantListEntry -> Lude.Maybe Lude.Text) (\s a -> s {grantId = a} :: GrantListEntry)
{-# DEPRECATED gleGrantId "Use generic-lens or generic-optics with 'grantId' instead." #-}

-- | A list of key-value pairs that must be present in the encryption context of certain subsequent operations that the grant allows.
--
-- /Note:/ Consider using 'constraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleConstraints :: Lens.Lens' GrantListEntry (Lude.Maybe GrantConstraints)
gleConstraints = Lens.lens (constraints :: GrantListEntry -> Lude.Maybe GrantConstraints) (\s a -> s {constraints = a} :: GrantListEntry)
{-# DEPRECATED gleConstraints "Use generic-lens or generic-optics with 'constraints' instead." #-}

-- | The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually contains the user or role designated as the grantee principal in the grant. However, when the grantee principal in the grant is an AWS service, the @GranteePrincipal@ field contains the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal> , which might represent several different grantee principals.
--
-- /Note:/ Consider using 'granteePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleGranteePrincipal :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Text)
gleGranteePrincipal = Lens.lens (granteePrincipal :: GrantListEntry -> Lude.Maybe Lude.Text) (\s a -> s {granteePrincipal = a} :: GrantListEntry)
{-# DEPRECATED gleGranteePrincipal "Use generic-lens or generic-optics with 'granteePrincipal' instead." #-}

-- | The friendly name that identifies the grant. If a name was provided in the 'CreateGrant' request, that name is returned. Otherwise this value is null.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleName :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Text)
gleName = Lens.lens (name :: GrantListEntry -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GrantListEntry)
{-# DEPRECATED gleName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time when the grant was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleCreationDate :: Lens.Lens' GrantListEntry (Lude.Maybe Lude.Timestamp)
gleCreationDate = Lens.lens (creationDate :: GrantListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: GrantListEntry)
{-# DEPRECATED gleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The list of operations permitted by the grant.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleOperations :: Lens.Lens' GrantListEntry (Lude.Maybe [GrantOperation])
gleOperations = Lens.lens (operations :: GrantListEntry -> Lude.Maybe [GrantOperation]) (\s a -> s {operations = a} :: GrantListEntry)
{-# DEPRECATED gleOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

instance Lude.FromJSON GrantListEntry where
  parseJSON =
    Lude.withObject
      "GrantListEntry"
      ( \x ->
          GrantListEntry'
            Lude.<$> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..:? "RetiringPrincipal")
            Lude.<*> (x Lude..:? "IssuingAccount")
            Lude.<*> (x Lude..:? "GrantId")
            Lude.<*> (x Lude..:? "Constraints")
            Lude.<*> (x Lude..:? "GranteePrincipal")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Operations" Lude..!= Lude.mempty)
      )
