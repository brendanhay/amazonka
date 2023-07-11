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
-- Module      : Amazonka.Inspector2.Types.DelegatedAdmin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.DelegatedAdmin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.RelationshipStatus
import qualified Amazonka.Prelude as Prelude

-- | Details of the Amazon Inspector delegated administrator for your
-- organization.
--
-- /See:/ 'newDelegatedAdmin' smart constructor.
data DelegatedAdmin = DelegatedAdmin'
  { -- | The Amazon Web Services account ID of the Amazon Inspector delegated
    -- administrator for your organization.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon Inspector delegated administrator.
    relationshipStatus :: Prelude.Maybe RelationshipStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelegatedAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'delegatedAdmin_accountId' - The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator for your organization.
--
-- 'relationshipStatus', 'delegatedAdmin_relationshipStatus' - The status of the Amazon Inspector delegated administrator.
newDelegatedAdmin ::
  DelegatedAdmin
newDelegatedAdmin =
  DelegatedAdmin'
    { accountId = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator for your organization.
delegatedAdmin_accountId :: Lens.Lens' DelegatedAdmin (Prelude.Maybe Prelude.Text)
delegatedAdmin_accountId = Lens.lens (\DelegatedAdmin' {accountId} -> accountId) (\s@DelegatedAdmin' {} a -> s {accountId = a} :: DelegatedAdmin)

-- | The status of the Amazon Inspector delegated administrator.
delegatedAdmin_relationshipStatus :: Lens.Lens' DelegatedAdmin (Prelude.Maybe RelationshipStatus)
delegatedAdmin_relationshipStatus = Lens.lens (\DelegatedAdmin' {relationshipStatus} -> relationshipStatus) (\s@DelegatedAdmin' {} a -> s {relationshipStatus = a} :: DelegatedAdmin)

instance Data.FromJSON DelegatedAdmin where
  parseJSON =
    Data.withObject
      "DelegatedAdmin"
      ( \x ->
          DelegatedAdmin'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "relationshipStatus")
      )

instance Prelude.Hashable DelegatedAdmin where
  hashWithSalt _salt DelegatedAdmin' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` relationshipStatus

instance Prelude.NFData DelegatedAdmin where
  rnf DelegatedAdmin' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf relationshipStatus
