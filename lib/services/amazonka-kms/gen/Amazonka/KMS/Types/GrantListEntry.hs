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
-- Module      : Amazonka.KMS.Types.GrantListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.GrantListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.GrantConstraints
import Amazonka.KMS.Types.GrantOperation
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a grant.
--
-- /See:/ 'newGrantListEntry' smart constructor.
data GrantListEntry = GrantListEntry'
  { -- | A list of key-value pairs that must be present in the encryption context
    -- of certain subsequent operations that the grant allows.
    constraints :: Prelude.Maybe GrantConstraints,
    -- | The date and time when the grant was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the grant.
    grantId :: Prelude.Maybe Prelude.Text,
    -- | The identity that gets the permissions in the grant.
    --
    -- The @GranteePrincipal@ field in the @ListGrants@ response usually
    -- contains the user or role designated as the grantee principal in the
    -- grant. However, when the grantee principal in the grant is an Amazon Web
    -- Services service, the @GranteePrincipal@ field contains the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
    -- which might represent several different grantee principals.
    granteePrincipal :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account under which the grant was issued.
    issuingAccount :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the KMS key to which the grant applies.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that identifies the grant. If a name was provided in
    -- the CreateGrant request, that name is returned. Otherwise this value is
    -- null.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of operations permitted by the grant.
    operations :: Prelude.Maybe [GrantOperation],
    -- | The principal that can retire the grant.
    retiringPrincipal :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'creationDate', 'grantListEntry_creationDate' - The date and time when the grant was created.
--
-- 'grantId', 'grantListEntry_grantId' - The unique identifier for the grant.
--
-- 'granteePrincipal', 'grantListEntry_granteePrincipal' - The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually
-- contains the user or role designated as the grantee principal in the
-- grant. However, when the grantee principal in the grant is an Amazon Web
-- Services service, the @GranteePrincipal@ field contains the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
-- which might represent several different grantee principals.
--
-- 'issuingAccount', 'grantListEntry_issuingAccount' - The Amazon Web Services account under which the grant was issued.
--
-- 'keyId', 'grantListEntry_keyId' - The unique identifier for the KMS key to which the grant applies.
--
-- 'name', 'grantListEntry_name' - The friendly name that identifies the grant. If a name was provided in
-- the CreateGrant request, that name is returned. Otherwise this value is
-- null.
--
-- 'operations', 'grantListEntry_operations' - The list of operations permitted by the grant.
--
-- 'retiringPrincipal', 'grantListEntry_retiringPrincipal' - The principal that can retire the grant.
newGrantListEntry ::
  GrantListEntry
newGrantListEntry =
  GrantListEntry'
    { constraints = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      grantId = Prelude.Nothing,
      granteePrincipal = Prelude.Nothing,
      issuingAccount = Prelude.Nothing,
      keyId = Prelude.Nothing,
      name = Prelude.Nothing,
      operations = Prelude.Nothing,
      retiringPrincipal = Prelude.Nothing
    }

-- | A list of key-value pairs that must be present in the encryption context
-- of certain subsequent operations that the grant allows.
grantListEntry_constraints :: Lens.Lens' GrantListEntry (Prelude.Maybe GrantConstraints)
grantListEntry_constraints = Lens.lens (\GrantListEntry' {constraints} -> constraints) (\s@GrantListEntry' {} a -> s {constraints = a} :: GrantListEntry)

-- | The date and time when the grant was created.
grantListEntry_creationDate :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.UTCTime)
grantListEntry_creationDate = Lens.lens (\GrantListEntry' {creationDate} -> creationDate) (\s@GrantListEntry' {} a -> s {creationDate = a} :: GrantListEntry) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the grant.
grantListEntry_grantId :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_grantId = Lens.lens (\GrantListEntry' {grantId} -> grantId) (\s@GrantListEntry' {} a -> s {grantId = a} :: GrantListEntry)

-- | The identity that gets the permissions in the grant.
--
-- The @GranteePrincipal@ field in the @ListGrants@ response usually
-- contains the user or role designated as the grantee principal in the
-- grant. However, when the grantee principal in the grant is an Amazon Web
-- Services service, the @GranteePrincipal@ field contains the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html#principal-services service principal>,
-- which might represent several different grantee principals.
grantListEntry_granteePrincipal :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_granteePrincipal = Lens.lens (\GrantListEntry' {granteePrincipal} -> granteePrincipal) (\s@GrantListEntry' {} a -> s {granteePrincipal = a} :: GrantListEntry)

-- | The Amazon Web Services account under which the grant was issued.
grantListEntry_issuingAccount :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_issuingAccount = Lens.lens (\GrantListEntry' {issuingAccount} -> issuingAccount) (\s@GrantListEntry' {} a -> s {issuingAccount = a} :: GrantListEntry)

-- | The unique identifier for the KMS key to which the grant applies.
grantListEntry_keyId :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_keyId = Lens.lens (\GrantListEntry' {keyId} -> keyId) (\s@GrantListEntry' {} a -> s {keyId = a} :: GrantListEntry)

-- | The friendly name that identifies the grant. If a name was provided in
-- the CreateGrant request, that name is returned. Otherwise this value is
-- null.
grantListEntry_name :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_name = Lens.lens (\GrantListEntry' {name} -> name) (\s@GrantListEntry' {} a -> s {name = a} :: GrantListEntry)

-- | The list of operations permitted by the grant.
grantListEntry_operations :: Lens.Lens' GrantListEntry (Prelude.Maybe [GrantOperation])
grantListEntry_operations = Lens.lens (\GrantListEntry' {operations} -> operations) (\s@GrantListEntry' {} a -> s {operations = a} :: GrantListEntry) Prelude.. Lens.mapping Lens.coerced

-- | The principal that can retire the grant.
grantListEntry_retiringPrincipal :: Lens.Lens' GrantListEntry (Prelude.Maybe Prelude.Text)
grantListEntry_retiringPrincipal = Lens.lens (\GrantListEntry' {retiringPrincipal} -> retiringPrincipal) (\s@GrantListEntry' {} a -> s {retiringPrincipal = a} :: GrantListEntry)

instance Data.FromJSON GrantListEntry where
  parseJSON =
    Data.withObject
      "GrantListEntry"
      ( \x ->
          GrantListEntry'
            Prelude.<$> (x Data..:? "Constraints")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "GrantId")
            Prelude.<*> (x Data..:? "GranteePrincipal")
            Prelude.<*> (x Data..:? "IssuingAccount")
            Prelude.<*> (x Data..:? "KeyId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Operations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RetiringPrincipal")
      )

instance Prelude.Hashable GrantListEntry where
  hashWithSalt _salt GrantListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` constraints
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` grantId
      `Prelude.hashWithSalt` granteePrincipal
      `Prelude.hashWithSalt` issuingAccount
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operations
      `Prelude.hashWithSalt` retiringPrincipal

instance Prelude.NFData GrantListEntry where
  rnf GrantListEntry' {..} =
    Prelude.rnf constraints
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf grantId
      `Prelude.seq` Prelude.rnf granteePrincipal
      `Prelude.seq` Prelude.rnf issuingAccount
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf retiringPrincipal
