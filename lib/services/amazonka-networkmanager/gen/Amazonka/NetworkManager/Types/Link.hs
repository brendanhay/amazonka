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
-- Module      : Amazonka.NetworkManager.Types.Link
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Link where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Bandwidth
import Amazonka.NetworkManager.Types.LinkState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a link.
--
-- /See:/ 'newLink' smart constructor.
data Link = Link'
  { -- | The bandwidth for the link.
    bandwidth :: Prelude.Maybe Bandwidth,
    -- | The date and time that the link was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of the link.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the link.
    linkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The provider of the link.
    provider :: Prelude.Maybe Prelude.Text,
    -- | The ID of the site.
    siteId :: Prelude.Maybe Prelude.Text,
    -- | The state of the link.
    state :: Prelude.Maybe LinkState,
    -- | The tags for the link.
    tags :: Prelude.Maybe [Tag],
    -- | The type of the link.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Link' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidth', 'link_bandwidth' - The bandwidth for the link.
--
-- 'createdAt', 'link_createdAt' - The date and time that the link was created.
--
-- 'description', 'link_description' - The description of the link.
--
-- 'globalNetworkId', 'link_globalNetworkId' - The ID of the global network.
--
-- 'linkArn', 'link_linkArn' - The Amazon Resource Name (ARN) of the link.
--
-- 'linkId', 'link_linkId' - The ID of the link.
--
-- 'provider', 'link_provider' - The provider of the link.
--
-- 'siteId', 'link_siteId' - The ID of the site.
--
-- 'state', 'link_state' - The state of the link.
--
-- 'tags', 'link_tags' - The tags for the link.
--
-- 'type'', 'link_type' - The type of the link.
newLink ::
  Link
newLink =
  Link'
    { bandwidth = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      linkArn = Prelude.Nothing,
      linkId = Prelude.Nothing,
      provider = Prelude.Nothing,
      siteId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The bandwidth for the link.
link_bandwidth :: Lens.Lens' Link (Prelude.Maybe Bandwidth)
link_bandwidth = Lens.lens (\Link' {bandwidth} -> bandwidth) (\s@Link' {} a -> s {bandwidth = a} :: Link)

-- | The date and time that the link was created.
link_createdAt :: Lens.Lens' Link (Prelude.Maybe Prelude.UTCTime)
link_createdAt = Lens.lens (\Link' {createdAt} -> createdAt) (\s@Link' {} a -> s {createdAt = a} :: Link) Prelude.. Lens.mapping Data._Time

-- | The description of the link.
link_description :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_description = Lens.lens (\Link' {description} -> description) (\s@Link' {} a -> s {description = a} :: Link)

-- | The ID of the global network.
link_globalNetworkId :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_globalNetworkId = Lens.lens (\Link' {globalNetworkId} -> globalNetworkId) (\s@Link' {} a -> s {globalNetworkId = a} :: Link)

-- | The Amazon Resource Name (ARN) of the link.
link_linkArn :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_linkArn = Lens.lens (\Link' {linkArn} -> linkArn) (\s@Link' {} a -> s {linkArn = a} :: Link)

-- | The ID of the link.
link_linkId :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_linkId = Lens.lens (\Link' {linkId} -> linkId) (\s@Link' {} a -> s {linkId = a} :: Link)

-- | The provider of the link.
link_provider :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_provider = Lens.lens (\Link' {provider} -> provider) (\s@Link' {} a -> s {provider = a} :: Link)

-- | The ID of the site.
link_siteId :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_siteId = Lens.lens (\Link' {siteId} -> siteId) (\s@Link' {} a -> s {siteId = a} :: Link)

-- | The state of the link.
link_state :: Lens.Lens' Link (Prelude.Maybe LinkState)
link_state = Lens.lens (\Link' {state} -> state) (\s@Link' {} a -> s {state = a} :: Link)

-- | The tags for the link.
link_tags :: Lens.Lens' Link (Prelude.Maybe [Tag])
link_tags = Lens.lens (\Link' {tags} -> tags) (\s@Link' {} a -> s {tags = a} :: Link) Prelude.. Lens.mapping Lens.coerced

-- | The type of the link.
link_type :: Lens.Lens' Link (Prelude.Maybe Prelude.Text)
link_type = Lens.lens (\Link' {type'} -> type') (\s@Link' {} a -> s {type' = a} :: Link)

instance Data.FromJSON Link where
  parseJSON =
    Data.withObject
      "Link"
      ( \x ->
          Link'
            Prelude.<$> (x Data..:? "Bandwidth")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "LinkArn")
            Prelude.<*> (x Data..:? "LinkId")
            Prelude.<*> (x Data..:? "Provider")
            Prelude.<*> (x Data..:? "SiteId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable Link where
  hashWithSalt _salt Link' {..} =
    _salt
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` linkArn
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Link where
  rnf Link' {..} =
    Prelude.rnf bandwidth `Prelude.seq`
      Prelude.rnf createdAt `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf globalNetworkId `Prelude.seq`
            Prelude.rnf linkArn `Prelude.seq`
              Prelude.rnf linkId `Prelude.seq`
                Prelude.rnf provider `Prelude.seq`
                  Prelude.rnf siteId `Prelude.seq`
                    Prelude.rnf state `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf type'
