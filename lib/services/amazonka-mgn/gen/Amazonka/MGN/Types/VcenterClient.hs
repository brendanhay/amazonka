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
-- Module      : Amazonka.MGN.Types.VcenterClient
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.VcenterClient where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | vCenter client.
--
-- /See:/ 'newVcenterClient' smart constructor.
data VcenterClient = VcenterClient'
  { -- | Arn of vCenter client.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Datacenter name of vCenter client.
    datacenterName :: Prelude.Maybe Prelude.Text,
    -- | Hostname of vCenter client .
    hostname :: Prelude.Maybe Prelude.Text,
    -- | Last seen time of vCenter client.
    lastSeenDatetime :: Prelude.Maybe Prelude.Text,
    -- | Tags for Source Server of vCenter client.
    sourceServerTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Tags for vCenter client.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | ID of vCenter client.
    vcenterClientID :: Prelude.Maybe Prelude.Text,
    -- | Vcenter UUID of vCenter client.
    vcenterUUID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VcenterClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'vcenterClient_arn' - Arn of vCenter client.
--
-- 'datacenterName', 'vcenterClient_datacenterName' - Datacenter name of vCenter client.
--
-- 'hostname', 'vcenterClient_hostname' - Hostname of vCenter client .
--
-- 'lastSeenDatetime', 'vcenterClient_lastSeenDatetime' - Last seen time of vCenter client.
--
-- 'sourceServerTags', 'vcenterClient_sourceServerTags' - Tags for Source Server of vCenter client.
--
-- 'tags', 'vcenterClient_tags' - Tags for vCenter client.
--
-- 'vcenterClientID', 'vcenterClient_vcenterClientID' - ID of vCenter client.
--
-- 'vcenterUUID', 'vcenterClient_vcenterUUID' - Vcenter UUID of vCenter client.
newVcenterClient ::
  VcenterClient
newVcenterClient =
  VcenterClient'
    { arn = Prelude.Nothing,
      datacenterName = Prelude.Nothing,
      hostname = Prelude.Nothing,
      lastSeenDatetime = Prelude.Nothing,
      sourceServerTags = Prelude.Nothing,
      tags = Prelude.Nothing,
      vcenterClientID = Prelude.Nothing,
      vcenterUUID = Prelude.Nothing
    }

-- | Arn of vCenter client.
vcenterClient_arn :: Lens.Lens' VcenterClient (Prelude.Maybe Prelude.Text)
vcenterClient_arn = Lens.lens (\VcenterClient' {arn} -> arn) (\s@VcenterClient' {} a -> s {arn = a} :: VcenterClient)

-- | Datacenter name of vCenter client.
vcenterClient_datacenterName :: Lens.Lens' VcenterClient (Prelude.Maybe Prelude.Text)
vcenterClient_datacenterName = Lens.lens (\VcenterClient' {datacenterName} -> datacenterName) (\s@VcenterClient' {} a -> s {datacenterName = a} :: VcenterClient)

-- | Hostname of vCenter client .
vcenterClient_hostname :: Lens.Lens' VcenterClient (Prelude.Maybe Prelude.Text)
vcenterClient_hostname = Lens.lens (\VcenterClient' {hostname} -> hostname) (\s@VcenterClient' {} a -> s {hostname = a} :: VcenterClient)

-- | Last seen time of vCenter client.
vcenterClient_lastSeenDatetime :: Lens.Lens' VcenterClient (Prelude.Maybe Prelude.Text)
vcenterClient_lastSeenDatetime = Lens.lens (\VcenterClient' {lastSeenDatetime} -> lastSeenDatetime) (\s@VcenterClient' {} a -> s {lastSeenDatetime = a} :: VcenterClient)

-- | Tags for Source Server of vCenter client.
vcenterClient_sourceServerTags :: Lens.Lens' VcenterClient (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
vcenterClient_sourceServerTags = Lens.lens (\VcenterClient' {sourceServerTags} -> sourceServerTags) (\s@VcenterClient' {} a -> s {sourceServerTags = a} :: VcenterClient) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Tags for vCenter client.
vcenterClient_tags :: Lens.Lens' VcenterClient (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
vcenterClient_tags = Lens.lens (\VcenterClient' {tags} -> tags) (\s@VcenterClient' {} a -> s {tags = a} :: VcenterClient) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | ID of vCenter client.
vcenterClient_vcenterClientID :: Lens.Lens' VcenterClient (Prelude.Maybe Prelude.Text)
vcenterClient_vcenterClientID = Lens.lens (\VcenterClient' {vcenterClientID} -> vcenterClientID) (\s@VcenterClient' {} a -> s {vcenterClientID = a} :: VcenterClient)

-- | Vcenter UUID of vCenter client.
vcenterClient_vcenterUUID :: Lens.Lens' VcenterClient (Prelude.Maybe Prelude.Text)
vcenterClient_vcenterUUID = Lens.lens (\VcenterClient' {vcenterUUID} -> vcenterUUID) (\s@VcenterClient' {} a -> s {vcenterUUID = a} :: VcenterClient)

instance Data.FromJSON VcenterClient where
  parseJSON =
    Data.withObject
      "VcenterClient"
      ( \x ->
          VcenterClient'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "datacenterName")
            Prelude.<*> (x Data..:? "hostname")
            Prelude.<*> (x Data..:? "lastSeenDatetime")
            Prelude.<*> ( x Data..:? "sourceServerTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vcenterClientID")
            Prelude.<*> (x Data..:? "vcenterUUID")
      )

instance Prelude.Hashable VcenterClient where
  hashWithSalt _salt VcenterClient' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` datacenterName
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` lastSeenDatetime
      `Prelude.hashWithSalt` sourceServerTags
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vcenterClientID
      `Prelude.hashWithSalt` vcenterUUID

instance Prelude.NFData VcenterClient where
  rnf VcenterClient' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf datacenterName
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf lastSeenDatetime
      `Prelude.seq` Prelude.rnf sourceServerTags
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vcenterClientID
      `Prelude.seq` Prelude.rnf vcenterUUID
