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
-- Module      : Amazonka.NetworkFirewall.Types.IPSetMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.IPSetMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | General information about the IP set.
--
-- /See:/ 'newIPSetMetadata' smart constructor.
data IPSetMetadata = IPSetMetadata'
  { -- | Describes the total number of CIDR blocks currently in use by the IP set
    -- references in a firewall. To determine how many CIDR blocks are
    -- available for you to use in a firewall, you can call
    -- @AvailableCIDRCount@.
    resolvedCIDRCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolvedCIDRCount', 'iPSetMetadata_resolvedCIDRCount' - Describes the total number of CIDR blocks currently in use by the IP set
-- references in a firewall. To determine how many CIDR blocks are
-- available for you to use in a firewall, you can call
-- @AvailableCIDRCount@.
newIPSetMetadata ::
  IPSetMetadata
newIPSetMetadata =
  IPSetMetadata' {resolvedCIDRCount = Prelude.Nothing}

-- | Describes the total number of CIDR blocks currently in use by the IP set
-- references in a firewall. To determine how many CIDR blocks are
-- available for you to use in a firewall, you can call
-- @AvailableCIDRCount@.
iPSetMetadata_resolvedCIDRCount :: Lens.Lens' IPSetMetadata (Prelude.Maybe Prelude.Natural)
iPSetMetadata_resolvedCIDRCount = Lens.lens (\IPSetMetadata' {resolvedCIDRCount} -> resolvedCIDRCount) (\s@IPSetMetadata' {} a -> s {resolvedCIDRCount = a} :: IPSetMetadata)

instance Data.FromJSON IPSetMetadata where
  parseJSON =
    Data.withObject
      "IPSetMetadata"
      ( \x ->
          IPSetMetadata'
            Prelude.<$> (x Data..:? "ResolvedCIDRCount")
      )

instance Prelude.Hashable IPSetMetadata where
  hashWithSalt _salt IPSetMetadata' {..} =
    _salt `Prelude.hashWithSalt` resolvedCIDRCount

instance Prelude.NFData IPSetMetadata where
  rnf IPSetMetadata' {..} =
    Prelude.rnf resolvedCIDRCount
