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
-- Module      : Amazonka.CloudSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.DomainEndpointOptionsStatus where

import Amazonka.CloudSearch.Types.DomainEndpointOptions
import Amazonka.CloudSearch.Types.OptionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration and status of the domain\'s endpoint options.
--
-- /See:/ 'newDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { -- | The domain endpoint options configured for the domain.
    options :: DomainEndpointOptions,
    -- | The status of the configured domain endpoint options.
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
-- 'options', 'domainEndpointOptionsStatus_options' - The domain endpoint options configured for the domain.
--
-- 'status', 'domainEndpointOptionsStatus_status' - The status of the configured domain endpoint options.
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

-- | The domain endpoint options configured for the domain.
domainEndpointOptionsStatus_options :: Lens.Lens' DomainEndpointOptionsStatus DomainEndpointOptions
domainEndpointOptionsStatus_options = Lens.lens (\DomainEndpointOptionsStatus' {options} -> options) (\s@DomainEndpointOptionsStatus' {} a -> s {options = a} :: DomainEndpointOptionsStatus)

-- | The status of the configured domain endpoint options.
domainEndpointOptionsStatus_status :: Lens.Lens' DomainEndpointOptionsStatus OptionStatus
domainEndpointOptionsStatus_status = Lens.lens (\DomainEndpointOptionsStatus' {status} -> status) (\s@DomainEndpointOptionsStatus' {} a -> s {status = a} :: DomainEndpointOptionsStatus)

instance Data.FromXML DomainEndpointOptionsStatus where
  parseXML x =
    DomainEndpointOptionsStatus'
      Prelude.<$> (x Data..@ "Options")
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable DomainEndpointOptionsStatus where
  hashWithSalt _salt DomainEndpointOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData DomainEndpointOptionsStatus where
  rnf DomainEndpointOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
