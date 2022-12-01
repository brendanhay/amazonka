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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.UpdateNetworkSiteResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.NetworkSite

-- | /See:/ 'newUpdateNetworkSiteResponse' smart constructor.
data UpdateNetworkSiteResponse = UpdateNetworkSiteResponse'
  { -- | The network site tags.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Information about the network site.
    networkSite :: Prelude.Maybe NetworkSite
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
-- 'tags', 'updateNetworkSiteResponse_tags' - The network site tags.
--
-- 'networkSite', 'updateNetworkSiteResponse_networkSite' - Information about the network site.
newUpdateNetworkSiteResponse ::
  UpdateNetworkSiteResponse
newUpdateNetworkSiteResponse =
  UpdateNetworkSiteResponse'
    { tags = Prelude.Nothing,
      networkSite = Prelude.Nothing
    }

-- | The network site tags.
updateNetworkSiteResponse_tags :: Lens.Lens' UpdateNetworkSiteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateNetworkSiteResponse_tags = Lens.lens (\UpdateNetworkSiteResponse' {tags} -> tags) (\s@UpdateNetworkSiteResponse' {} a -> s {tags = a} :: UpdateNetworkSiteResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Information about the network site.
updateNetworkSiteResponse_networkSite :: Lens.Lens' UpdateNetworkSiteResponse (Prelude.Maybe NetworkSite)
updateNetworkSiteResponse_networkSite = Lens.lens (\UpdateNetworkSiteResponse' {networkSite} -> networkSite) (\s@UpdateNetworkSiteResponse' {} a -> s {networkSite = a} :: UpdateNetworkSiteResponse)

instance Core.FromJSON UpdateNetworkSiteResponse where
  parseJSON =
    Core.withObject
      "UpdateNetworkSiteResponse"
      ( \x ->
          UpdateNetworkSiteResponse'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "networkSite")
      )

instance Prelude.Hashable UpdateNetworkSiteResponse where
  hashWithSalt _salt UpdateNetworkSiteResponse' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` networkSite

instance Prelude.NFData UpdateNetworkSiteResponse where
  rnf UpdateNetworkSiteResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf networkSite
