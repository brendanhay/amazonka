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
-- Module      : Amazonka.OpenSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainEndpointOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.DomainEndpointOptions
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | The configured endpoint options for a domain and their current status.
--
-- /See:/ 'newDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { -- | Options to configure the endpoint for a domain.
    options :: DomainEndpointOptions,
    -- | The status of the endpoint options for a domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainEndpointOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'domainEndpointOptionsStatus_options' - Options to configure the endpoint for a domain.
--
-- 'status', 'domainEndpointOptionsStatus_status' - The status of the endpoint options for a domain.
newDomainEndpointOptionsStatus ::
  -- | 'options'
  DomainEndpointOptions ->
  -- | 'status'
  OptionStatus ->
  DomainEndpointOptionsStatus
newDomainEndpointOptionsStatus pOptions_ pStatus_ =
  DomainEndpointOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Options to configure the endpoint for a domain.
domainEndpointOptionsStatus_options :: Lens.Lens' DomainEndpointOptionsStatus DomainEndpointOptions
domainEndpointOptionsStatus_options = Lens.lens (\DomainEndpointOptionsStatus' {options} -> options) (\s@DomainEndpointOptionsStatus' {} a -> s {options = a} :: DomainEndpointOptionsStatus)

-- | The status of the endpoint options for a domain.
domainEndpointOptionsStatus_status :: Lens.Lens' DomainEndpointOptionsStatus OptionStatus
domainEndpointOptionsStatus_status = Lens.lens (\DomainEndpointOptionsStatus' {status} -> status) (\s@DomainEndpointOptionsStatus' {} a -> s {status = a} :: DomainEndpointOptionsStatus)

instance Data.FromJSON DomainEndpointOptionsStatus where
  parseJSON =
    Data.withObject
      "DomainEndpointOptionsStatus"
      ( \x ->
          DomainEndpointOptionsStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable DomainEndpointOptionsStatus where
  hashWithSalt _salt DomainEndpointOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData DomainEndpointOptionsStatus where
  rnf DomainEndpointOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
