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
-- Module      : Network.AWS.Route53Domains.Types.DomainTransferability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainTransferability where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53Domains.Types.Transferable

-- | A complex type that contains information about whether the specified
-- domain can be transferred to Route 53.
--
-- /See:/ 'newDomainTransferability' smart constructor.
data DomainTransferability = DomainTransferability'
  { transferable :: Prelude.Maybe Transferable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON DomainTransferability where
  parseJSON =
    Prelude.withObject
      "DomainTransferability"
      ( \x ->
          DomainTransferability'
            Prelude.<$> (x Prelude..:? "Transferable")
      )

instance Prelude.Hashable DomainTransferability

instance Prelude.NFData DomainTransferability
