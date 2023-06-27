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
-- Module      : Amazonka.WorkMail.Types.OrganizationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.OrganizationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The representation of an organization.
--
-- /See:/ 'newOrganizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { -- | The alias associated with the organization.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The default email domain associated with the organization.
    defaultMailDomain :: Prelude.Maybe Prelude.Text,
    -- | The error message associated with the organization. It is only present
    -- if unexpected behavior has occurred with regards to the organization. It
    -- provides insight or solutions regarding unexpected behavior.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier associated with the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The state associated with the organization.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'organizationSummary_alias' - The alias associated with the organization.
--
-- 'defaultMailDomain', 'organizationSummary_defaultMailDomain' - The default email domain associated with the organization.
--
-- 'errorMessage', 'organizationSummary_errorMessage' - The error message associated with the organization. It is only present
-- if unexpected behavior has occurred with regards to the organization. It
-- provides insight or solutions regarding unexpected behavior.
--
-- 'organizationId', 'organizationSummary_organizationId' - The identifier associated with the organization.
--
-- 'state', 'organizationSummary_state' - The state associated with the organization.
newOrganizationSummary ::
  OrganizationSummary
newOrganizationSummary =
  OrganizationSummary'
    { alias = Prelude.Nothing,
      defaultMailDomain = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The alias associated with the organization.
organizationSummary_alias :: Lens.Lens' OrganizationSummary (Prelude.Maybe Prelude.Text)
organizationSummary_alias = Lens.lens (\OrganizationSummary' {alias} -> alias) (\s@OrganizationSummary' {} a -> s {alias = a} :: OrganizationSummary)

-- | The default email domain associated with the organization.
organizationSummary_defaultMailDomain :: Lens.Lens' OrganizationSummary (Prelude.Maybe Prelude.Text)
organizationSummary_defaultMailDomain = Lens.lens (\OrganizationSummary' {defaultMailDomain} -> defaultMailDomain) (\s@OrganizationSummary' {} a -> s {defaultMailDomain = a} :: OrganizationSummary)

-- | The error message associated with the organization. It is only present
-- if unexpected behavior has occurred with regards to the organization. It
-- provides insight or solutions regarding unexpected behavior.
organizationSummary_errorMessage :: Lens.Lens' OrganizationSummary (Prelude.Maybe Prelude.Text)
organizationSummary_errorMessage = Lens.lens (\OrganizationSummary' {errorMessage} -> errorMessage) (\s@OrganizationSummary' {} a -> s {errorMessage = a} :: OrganizationSummary)

-- | The identifier associated with the organization.
organizationSummary_organizationId :: Lens.Lens' OrganizationSummary (Prelude.Maybe Prelude.Text)
organizationSummary_organizationId = Lens.lens (\OrganizationSummary' {organizationId} -> organizationId) (\s@OrganizationSummary' {} a -> s {organizationId = a} :: OrganizationSummary)

-- | The state associated with the organization.
organizationSummary_state :: Lens.Lens' OrganizationSummary (Prelude.Maybe Prelude.Text)
organizationSummary_state = Lens.lens (\OrganizationSummary' {state} -> state) (\s@OrganizationSummary' {} a -> s {state = a} :: OrganizationSummary)

instance Data.FromJSON OrganizationSummary where
  parseJSON =
    Data.withObject
      "OrganizationSummary"
      ( \x ->
          OrganizationSummary'
            Prelude.<$> (x Data..:? "Alias")
            Prelude.<*> (x Data..:? "DefaultMailDomain")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "OrganizationId")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable OrganizationSummary where
  hashWithSalt _salt OrganizationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` defaultMailDomain
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` state

instance Prelude.NFData OrganizationSummary where
  rnf OrganizationSummary' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf defaultMailDomain
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf state
