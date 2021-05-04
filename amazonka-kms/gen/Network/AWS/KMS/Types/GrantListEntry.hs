{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantOperation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a grant.
--
-- /See:/ 'newGrantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
  { -- | A list of key-value pairs that must be present in the encryption context
    -- of certain subsequent operations that the grant allows.
    constraints :: Prelude.Maybe GrantConstraints,
    -- | The list of operations permitted by the grant.
    operations :: Prelude.Maybe [GrantOperation],
    -- | The date and time when the grant was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The friendly name that identifies the grant. If a name was provided in
    -- the CreateGrant request, that name is returned. Otherwise this value is
    -- null.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identity that gets the permissions in the grant.
    --
    -- The @GranteePrincipal@ field in the @ListGrants@ response usually
    -- contains the user or role designated as the grantee principal in the
    -- grant. However, when the grantee principal in the grant is an AWS
    -- service, the @GranteePrincipal@ field contains the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
    -- which might represent several different grantee principals.
    granteePrincipal :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the grant.
    grantId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account under which the grant was issued.
    issuingAccount :: Prelude.Maybe Prelude.Text,
    -- | The principal that can retire the grant.
    retiringPrincipal :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the customer master key (CMK) to which the
    -- grant applies.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { constraints = Prelude.Nothing,
      operations = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      name = Prelude.Nothing,
      granteePrincipal = Prelude.Nothing,
      grantId = Prelude.Nothing,
      issuingAccount = Prelude.Nothing,
      retiringPrincipal = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | A list of key-value pairs that must be present in the encryption context
-- of certain subsequent operations that the grant allows.
grantListEntry_constraints :: Lens.Lens' GrantListEntry (Prelude.Maybe GrantConstraints)
grantListEntry_constraints = Lens.lens (\GrantListEntry' {constraints} -> constraints) (\s@GrantListEntry' {} a -> s {constraints = a} :: GrantListEntry)

-- | The list of operations permitted by the grant.
grantListEntry_operations :: Lens.Lens' GrantListEntry (Prelude.Maybe [GrantOperation])
grantListEntry_operations = Lens.lens (\GrantListEntry' {operations} -> operations) (\s@GrantListEntry' {} a -> s {operations = a} :: GrantListEntry) Prelude.. Lens.mapping Prelude._Coerce

-- | The date and time when the grant was created.
grantListEntry_creationDate :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.UTCTime)
grantListEntry_creationDate = Lens.lens (\GrantListEntry' {creationDate} -> creationDate) (\s@GrantListEntry' {} a -> s {creationDate = a} :: GrantListEntry) Prelude.. Lens.mapping Prelude._Time

-- | The friendly name that identifies the grant. If a name was provided in
-- the CreateGrant request, that name is returned. Otherwise this value is
-- null.
grantListEntry_name :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_name = Lens.lens (\GrantListEntry' {name} -> name) (\s@GrantListEntry' {} a -> s {name = a} :: GrantListEntry)

-- | The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually
-- contains the user or role designated as the grantee principal in the
-- grant. However, when the grantee principal in the grant is an AWS
-- service, the @GranteePrincipal@ field contains the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
-- which might represent several different grantee principals.
grantListEntry_granteePrincipal :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_granteePrincipal = Lens.lens (\GrantListEntry' {granteePrincipal} -> granteePrincipal) (\s@GrantListEntry' {} a -> s {granteePrincipal = a} :: GrantListEntry)

-- | The unique identifier for the grant.
grantListEntry_grantId :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_grantId = Lens.lens (\GrantListEntry' {grantId} -> grantId) (\s@GrantListEntry' {} a -> s {grantId = a} :: GrantListEntry)

-- | The AWS account under which the grant was issued.
grantListEntry_issuingAccount :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_issuingAccount = Lens.lens (\GrantListEntry' {issuingAccount} -> issuingAccount) (\s@GrantListEntry' {} a -> s {issuingAccount = a} :: GrantListEntry)

-- | The principal that can retire the grant.
grantListEntry_retiringPrincipal :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_retiringPrincipal = Lens.lens (\GrantListEntry' {retiringPrincipal} -> retiringPrincipal) (\s@GrantListEntry' {} a -> s {retiringPrincipal = a} :: GrantListEntry)

-- | The unique identifier for the customer master key (CMK) to which the
-- grant applies.
grantListEntry_keyId :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_keyId = Lens.lens (\GrantListEntry' {keyId} -> keyId) (\s@GrantListEntry' {} a -> s {keyId = a} :: GrantListEntry)

instance Prelude.FromJSON GrantListEntry where
  parseJSON =
    Prelude.withObject
      "GrantListEntry"
      ( \x ->
          GrantListEntry'
            Prelude.<$> (x Prelude..:? "Constraints")
            Prelude.<*> ( x Prelude..:? "Operations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "GranteePrincipal")
            Prelude.<*> (x Prelude..:? "GrantId")
            Prelude.<*> (x Prelude..:? "IssuingAccount")
            Prelude.<*> (x Prelude..:? "RetiringPrincipal")
            Prelude.<*> (x Prelude..:? "KeyId")
      )

instance Prelude.Hashable GrantListEntry

instance Prelude.NFData GrantListEntry
