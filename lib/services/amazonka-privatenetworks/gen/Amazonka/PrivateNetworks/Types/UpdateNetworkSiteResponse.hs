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
-- Module      : Amazonka.PrivateNetworks.Types.UpdateNetworkSiteResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.UpdateNetworkSiteResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.NetworkSite

-- | /See:/ 'newUpdateNetworkSiteResponse' smart constructor.
data UpdateNetworkSiteResponse = UpdateNetworkSiteResponse'
  { -- | Information about the network site.
    networkSite :: Prelude.Maybe NetworkSite,
    -- | The network site tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNetworkSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSite', 'updateNetworkSiteResponse_networkSite' - Information about the network site.
--
-- 'tags', 'updateNetworkSiteResponse_tags' - The network site tags.
newUpdateNetworkSiteResponse ::
  UpdateNetworkSiteResponse
newUpdateNetworkSiteResponse =
  UpdateNetworkSiteResponse'
    { networkSite =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Information about the network site.
updateNetworkSiteResponse_networkSite :: Lens.Lens' UpdateNetworkSiteResponse (Prelude.Maybe NetworkSite)
updateNetworkSiteResponse_networkSite = Lens.lens (\UpdateNetworkSiteResponse' {networkSite} -> networkSite) (\s@UpdateNetworkSiteResponse' {} a -> s {networkSite = a} :: UpdateNetworkSiteResponse)

-- | The network site tags.
updateNetworkSiteResponse_tags :: Lens.Lens' UpdateNetworkSiteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateNetworkSiteResponse_tags = Lens.lens (\UpdateNetworkSiteResponse' {tags} -> tags) (\s@UpdateNetworkSiteResponse' {} a -> s {tags = a} :: UpdateNetworkSiteResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON UpdateNetworkSiteResponse where
  parseJSON =
    Data.withObject
      "UpdateNetworkSiteResponse"
      ( \x ->
          UpdateNetworkSiteResponse'
            Prelude.<$> (x Data..:? "networkSite")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UpdateNetworkSiteResponse where
  hashWithSalt _salt UpdateNetworkSiteResponse' {..} =
    _salt
      `Prelude.hashWithSalt` networkSite
      `Prelude.hashWithSalt` tags

instance Prelude.NFData UpdateNetworkSiteResponse where
  rnf UpdateNetworkSiteResponse' {..} =
    Prelude.rnf networkSite
      `Prelude.seq` Prelude.rnf tags
