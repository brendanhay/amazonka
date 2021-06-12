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
-- Module      : Network.AWS.WorkMail.Types.OrganizationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.OrganizationSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The representation of an organization.
--
-- /See:/ 'newOrganizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { -- | The identifier associated with the organization.
    organizationId :: Core.Maybe Core.Text,
    -- | The alias associated with the organization.
    alias :: Core.Maybe Core.Text,
    -- | The default email domain associated with the organization.
    defaultMailDomain :: Core.Maybe Core.Text,
    -- | The state associated with the organization.
    state :: Core.Maybe Core.Text,
    -- | The error message associated with the organization. It is only present
    -- if unexpected behavior has occurred with regards to the organization. It
    -- provides insight or solutions regarding unexpected behavior.
    errorMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrganizationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'organizationSummary_organizationId' - The identifier associated with the organization.
--
-- 'alias', 'organizationSummary_alias' - The alias associated with the organization.
--
-- 'defaultMailDomain', 'organizationSummary_defaultMailDomain' - The default email domain associated with the organization.
--
-- 'state', 'organizationSummary_state' - The state associated with the organization.
--
-- 'errorMessage', 'organizationSummary_errorMessage' - The error message associated with the organization. It is only present
-- if unexpected behavior has occurred with regards to the organization. It
-- provides insight or solutions regarding unexpected behavior.
newOrganizationSummary ::
  OrganizationSummary
newOrganizationSummary =
  OrganizationSummary'
    { organizationId = Core.Nothing,
      alias = Core.Nothing,
      defaultMailDomain = Core.Nothing,
      state = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The identifier associated with the organization.
organizationSummary_organizationId :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
organizationSummary_organizationId = Lens.lens (\OrganizationSummary' {organizationId} -> organizationId) (\s@OrganizationSummary' {} a -> s {organizationId = a} :: OrganizationSummary)

-- | The alias associated with the organization.
organizationSummary_alias :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
organizationSummary_alias = Lens.lens (\OrganizationSummary' {alias} -> alias) (\s@OrganizationSummary' {} a -> s {alias = a} :: OrganizationSummary)

-- | The default email domain associated with the organization.
organizationSummary_defaultMailDomain :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
organizationSummary_defaultMailDomain = Lens.lens (\OrganizationSummary' {defaultMailDomain} -> defaultMailDomain) (\s@OrganizationSummary' {} a -> s {defaultMailDomain = a} :: OrganizationSummary)

-- | The state associated with the organization.
organizationSummary_state :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
organizationSummary_state = Lens.lens (\OrganizationSummary' {state} -> state) (\s@OrganizationSummary' {} a -> s {state = a} :: OrganizationSummary)

-- | The error message associated with the organization. It is only present
-- if unexpected behavior has occurred with regards to the organization. It
-- provides insight or solutions regarding unexpected behavior.
organizationSummary_errorMessage :: Lens.Lens' OrganizationSummary (Core.Maybe Core.Text)
organizationSummary_errorMessage = Lens.lens (\OrganizationSummary' {errorMessage} -> errorMessage) (\s@OrganizationSummary' {} a -> s {errorMessage = a} :: OrganizationSummary)

instance Core.FromJSON OrganizationSummary where
  parseJSON =
    Core.withObject
      "OrganizationSummary"
      ( \x ->
          OrganizationSummary'
            Core.<$> (x Core..:? "OrganizationId")
            Core.<*> (x Core..:? "Alias")
            Core.<*> (x Core..:? "DefaultMailDomain")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "ErrorMessage")
      )

instance Core.Hashable OrganizationSummary

instance Core.NFData OrganizationSummary
