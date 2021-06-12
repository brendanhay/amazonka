{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantListEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantOperation
import qualified Network.AWS.Lens as Lens

-- | Contains information about a grant.
--
-- /See:/ 'newGrantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
  { -- | A list of key-value pairs that must be present in the encryption context
    -- of certain subsequent operations that the grant allows.
    constraints :: Core.Maybe GrantConstraints,
    -- | The list of operations permitted by the grant.
    operations :: Core.Maybe [GrantOperation],
    -- | The date and time when the grant was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The friendly name that identifies the grant. If a name was provided in
    -- the CreateGrant request, that name is returned. Otherwise this value is
    -- null.
    name :: Core.Maybe Core.Text,
    -- | The identity that gets the permissions in the grant.
    --
    -- The @GranteePrincipal@ field in the @ListGrants@ response usually
    -- contains the user or role designated as the grantee principal in the
    -- grant. However, when the grantee principal in the grant is an AWS
    -- service, the @GranteePrincipal@ field contains the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
    -- which might represent several different grantee principals.
    granteePrincipal :: Core.Maybe Core.Text,
    -- | The unique identifier for the grant.
    grantId :: Core.Maybe Core.Text,
    -- | The AWS account under which the grant was issued.
    issuingAccount :: Core.Maybe Core.Text,
    -- | The principal that can retire the grant.
    retiringPrincipal :: Core.Maybe Core.Text,
    -- | The unique identifier for the customer master key (CMK) to which the
    -- grant applies.
    keyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GrantListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraints', 'grantListEntry_constraints' - A list of key-value pairs that must be present in the encryption context
-- of certain subsequent operations that the grant allows.
--
-- 'operations', 'grantListEntry_operations' - The list of operations permitted by the grant.
--
-- 'creationDate', 'grantListEntry_creationDate' - The date and time when the grant was created.
--
-- 'name', 'grantListEntry_name' - The friendly name that identifies the grant. If a name was provided in
-- the CreateGrant request, that name is returned. Otherwise this value is
-- null.
--
-- 'granteePrincipal', 'grantListEntry_granteePrincipal' - The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually
-- contains the user or role designated as the grantee principal in the
-- grant. However, when the grantee principal in the grant is an AWS
-- service, the @GranteePrincipal@ field contains the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
-- which might represent several different grantee principals.
--
-- 'grantId', 'grantListEntry_grantId' - The unique identifier for the grant.
--
-- 'issuingAccount', 'grantListEntry_issuingAccount' - The AWS account under which the grant was issued.
--
-- 'retiringPrincipal', 'grantListEntry_retiringPrincipal' - The principal that can retire the grant.
--
-- 'keyId', 'grantListEntry_keyId' - The unique identifier for the customer master key (CMK) to which the
-- grant applies.
newGrantListEntry ::
  GrantListEntry
newGrantListEntry =
  GrantListEntry'
    { constraints = Core.Nothing,
      operations = Core.Nothing,
      creationDate = Core.Nothing,
      name = Core.Nothing,
      granteePrincipal = Core.Nothing,
      grantId = Core.Nothing,
      issuingAccount = Core.Nothing,
      retiringPrincipal = Core.Nothing,
      keyId = Core.Nothing
    }

-- | A list of key-value pairs that must be present in the encryption context
-- of certain subsequent operations that the grant allows.
grantListEntry_constraints :: Lens.Lens' GrantListEntry (Core.Maybe GrantConstraints)
grantListEntry_constraints = Lens.lens (\GrantListEntry' {constraints} -> constraints) (\s@GrantListEntry' {} a -> s {constraints = a} :: GrantListEntry)

-- | The list of operations permitted by the grant.
grantListEntry_operations :: Lens.Lens' GrantListEntry (Core.Maybe [GrantOperation])
grantListEntry_operations = Lens.lens (\GrantListEntry' {operations} -> operations) (\s@GrantListEntry' {} a -> s {operations = a} :: GrantListEntry) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the grant was created.
grantListEntry_creationDate :: Lens.Lens' GrantListEntry (Core.Maybe Core.UTCTime)
grantListEntry_creationDate = Lens.lens (\GrantListEntry' {creationDate} -> creationDate) (\s@GrantListEntry' {} a -> s {creationDate = a} :: GrantListEntry) Core.. Lens.mapping Core._Time

-- | The friendly name that identifies the grant. If a name was provided in
-- the CreateGrant request, that name is returned. Otherwise this value is
-- null.
grantListEntry_name :: Lens.Lens' GrantListEntry (Core.Maybe Core.Text)
grantListEntry_name = Lens.lens (\GrantListEntry' {name} -> name) (\s@GrantListEntry' {} a -> s {name = a} :: GrantListEntry)

-- | The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually
-- contains the user or role designated as the grantee principal in the
-- grant. However, when the grantee principal in the grant is an AWS
-- service, the @GranteePrincipal@ field contains the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
-- which might represent several different grantee principals.
grantListEntry_granteePrincipal :: Lens.Lens' GrantListEntry (Core.Maybe Core.Text)
grantListEntry_granteePrincipal = Lens.lens (\GrantListEntry' {granteePrincipal} -> granteePrincipal) (\s@GrantListEntry' {} a -> s {granteePrincipal = a} :: GrantListEntry)

-- | The unique identifier for the grant.
grantListEntry_grantId :: Lens.Lens' GrantListEntry (Core.Maybe Core.Text)
grantListEntry_grantId = Lens.lens (\GrantListEntry' {grantId} -> grantId) (\s@GrantListEntry' {} a -> s {grantId = a} :: GrantListEntry)

-- | The AWS account under which the grant was issued.
grantListEntry_issuingAccount :: Lens.Lens' GrantListEntry (Core.Maybe Core.Text)
grantListEntry_issuingAccount = Lens.lens (\GrantListEntry' {issuingAccount} -> issuingAccount) (\s@GrantListEntry' {} a -> s {issuingAccount = a} :: GrantListEntry)

-- | The principal that can retire the grant.
grantListEntry_retiringPrincipal :: Lens.Lens' GrantListEntry (Core.Maybe Core.Text)
grantListEntry_retiringPrincipal = Lens.lens (\GrantListEntry' {retiringPrincipal} -> retiringPrincipal) (\s@GrantListEntry' {} a -> s {retiringPrincipal = a} :: GrantListEntry)

-- | The unique identifier for the customer master key (CMK) to which the
-- grant applies.
grantListEntry_keyId :: Lens.Lens' GrantListEntry (Core.Maybe Core.Text)
grantListEntry_keyId = Lens.lens (\GrantListEntry' {keyId} -> keyId) (\s@GrantListEntry' {} a -> s {keyId = a} :: GrantListEntry)

instance Core.FromJSON GrantListEntry where
  parseJSON =
    Core.withObject
      "GrantListEntry"
      ( \x ->
          GrantListEntry'
            Core.<$> (x Core..:? "Constraints")
            Core.<*> (x Core..:? "Operations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "GranteePrincipal")
            Core.<*> (x Core..:? "GrantId")
            Core.<*> (x Core..:? "IssuingAccount")
            Core.<*> (x Core..:? "RetiringPrincipal")
            Core.<*> (x Core..:? "KeyId")
      )

instance Core.Hashable GrantListEntry

instance Core.NFData GrantListEntry
