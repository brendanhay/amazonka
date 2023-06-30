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
-- Module      : Amazonka.FMS.Types.DiscoveredResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.DiscoveredResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource in the organization that\'s available to be associated with a
-- Firewall Manager resource set.
--
-- /See:/ 'newDiscoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { -- | The Amazon Web Services account ID associated with the discovered
    -- resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The name of the discovered resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the discovered resource.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The universal resource identifier (URI) of the discovered resource.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoveredResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'discoveredResource_accountId' - The Amazon Web Services account ID associated with the discovered
-- resource.
--
-- 'name', 'discoveredResource_name' - The name of the discovered resource.
--
-- 'type'', 'discoveredResource_type' - The type of the discovered resource.
--
-- 'uri', 'discoveredResource_uri' - The universal resource identifier (URI) of the discovered resource.
newDiscoveredResource ::
  DiscoveredResource
newDiscoveredResource =
  DiscoveredResource'
    { accountId = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | The Amazon Web Services account ID associated with the discovered
-- resource.
discoveredResource_accountId :: Lens.Lens' DiscoveredResource (Prelude.Maybe Prelude.Text)
discoveredResource_accountId = Lens.lens (\DiscoveredResource' {accountId} -> accountId) (\s@DiscoveredResource' {} a -> s {accountId = a} :: DiscoveredResource)

-- | The name of the discovered resource.
discoveredResource_name :: Lens.Lens' DiscoveredResource (Prelude.Maybe Prelude.Text)
discoveredResource_name = Lens.lens (\DiscoveredResource' {name} -> name) (\s@DiscoveredResource' {} a -> s {name = a} :: DiscoveredResource)

-- | The type of the discovered resource.
discoveredResource_type :: Lens.Lens' DiscoveredResource (Prelude.Maybe Prelude.Text)
discoveredResource_type = Lens.lens (\DiscoveredResource' {type'} -> type') (\s@DiscoveredResource' {} a -> s {type' = a} :: DiscoveredResource)

-- | The universal resource identifier (URI) of the discovered resource.
discoveredResource_uri :: Lens.Lens' DiscoveredResource (Prelude.Maybe Prelude.Text)
discoveredResource_uri = Lens.lens (\DiscoveredResource' {uri} -> uri) (\s@DiscoveredResource' {} a -> s {uri = a} :: DiscoveredResource)

instance Data.FromJSON DiscoveredResource where
  parseJSON =
    Data.withObject
      "DiscoveredResource"
      ( \x ->
          DiscoveredResource'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "URI")
      )

instance Prelude.Hashable DiscoveredResource where
  hashWithSalt _salt DiscoveredResource' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` uri

instance Prelude.NFData DiscoveredResource where
  rnf DiscoveredResource' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf uri
