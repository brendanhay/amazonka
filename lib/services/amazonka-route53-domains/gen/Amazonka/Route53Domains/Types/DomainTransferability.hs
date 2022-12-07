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
-- Module      : Amazonka.Route53Domains.Types.DomainTransferability
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.DomainTransferability where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.Transferable

-- | A complex type that contains information about whether the specified
-- domain can be transferred to Route 53.
--
-- /See:/ 'newDomainTransferability' smart constructor.
data DomainTransferability = DomainTransferability'
  { transferable :: Prelude.Maybe Transferable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainTransferability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferable', 'domainTransferability_transferable' - Undocumented member.
newDomainTransferability ::
  DomainTransferability
newDomainTransferability =
  DomainTransferability'
    { transferable =
        Prelude.Nothing
    }

-- | Undocumented member.
domainTransferability_transferable :: Lens.Lens' DomainTransferability (Prelude.Maybe Transferable)
domainTransferability_transferable = Lens.lens (\DomainTransferability' {transferable} -> transferable) (\s@DomainTransferability' {} a -> s {transferable = a} :: DomainTransferability)

instance Data.FromJSON DomainTransferability where
  parseJSON =
    Data.withObject
      "DomainTransferability"
      ( \x ->
          DomainTransferability'
            Prelude.<$> (x Data..:? "Transferable")
      )

instance Prelude.Hashable DomainTransferability where
  hashWithSalt _salt DomainTransferability' {..} =
    _salt `Prelude.hashWithSalt` transferable

instance Prelude.NFData DomainTransferability where
  rnf DomainTransferability' {..} =
    Prelude.rnf transferable
