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
-- Module      : Amazonka.DataSync.Types.QopConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.QopConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.HdfsDataTransferProtection
import Amazonka.DataSync.Types.HdfsRpcProtection
import qualified Amazonka.Prelude as Prelude

-- | The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer privacy settings configured on
-- the Hadoop Distributed File System (HDFS) cluster.
--
-- /See:/ 'newQopConfiguration' smart constructor.
data QopConfiguration = QopConfiguration'
  { -- | The data transfer protection setting configured on the HDFS cluster.
    -- This setting corresponds to your @dfs.data.transfer.protection@ setting
    -- in the @hdfs-site.xml@ file on your Hadoop cluster.
    dataTransferProtection :: Prelude.Maybe HdfsDataTransferProtection,
    -- | The RPC protection setting configured on the HDFS cluster. This setting
    -- corresponds to your @hadoop.rpc.protection@ setting in your
    -- @core-site.xml@ file on your Hadoop cluster.
    rpcProtection :: Prelude.Maybe HdfsRpcProtection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QopConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTransferProtection', 'qopConfiguration_dataTransferProtection' - The data transfer protection setting configured on the HDFS cluster.
-- This setting corresponds to your @dfs.data.transfer.protection@ setting
-- in the @hdfs-site.xml@ file on your Hadoop cluster.
--
-- 'rpcProtection', 'qopConfiguration_rpcProtection' - The RPC protection setting configured on the HDFS cluster. This setting
-- corresponds to your @hadoop.rpc.protection@ setting in your
-- @core-site.xml@ file on your Hadoop cluster.
newQopConfiguration ::
  QopConfiguration
newQopConfiguration =
  QopConfiguration'
    { dataTransferProtection =
        Prelude.Nothing,
      rpcProtection = Prelude.Nothing
    }

-- | The data transfer protection setting configured on the HDFS cluster.
-- This setting corresponds to your @dfs.data.transfer.protection@ setting
-- in the @hdfs-site.xml@ file on your Hadoop cluster.
qopConfiguration_dataTransferProtection :: Lens.Lens' QopConfiguration (Prelude.Maybe HdfsDataTransferProtection)
qopConfiguration_dataTransferProtection = Lens.lens (\QopConfiguration' {dataTransferProtection} -> dataTransferProtection) (\s@QopConfiguration' {} a -> s {dataTransferProtection = a} :: QopConfiguration)

-- | The RPC protection setting configured on the HDFS cluster. This setting
-- corresponds to your @hadoop.rpc.protection@ setting in your
-- @core-site.xml@ file on your Hadoop cluster.
qopConfiguration_rpcProtection :: Lens.Lens' QopConfiguration (Prelude.Maybe HdfsRpcProtection)
qopConfiguration_rpcProtection = Lens.lens (\QopConfiguration' {rpcProtection} -> rpcProtection) (\s@QopConfiguration' {} a -> s {rpcProtection = a} :: QopConfiguration)

instance Data.FromJSON QopConfiguration where
  parseJSON =
    Data.withObject
      "QopConfiguration"
      ( \x ->
          QopConfiguration'
            Prelude.<$> (x Data..:? "DataTransferProtection")
            Prelude.<*> (x Data..:? "RpcProtection")
      )

instance Prelude.Hashable QopConfiguration where
  hashWithSalt _salt QopConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferProtection
      `Prelude.hashWithSalt` rpcProtection

instance Prelude.NFData QopConfiguration where
  rnf QopConfiguration' {..} =
    Prelude.rnf dataTransferProtection
      `Prelude.seq` Prelude.rnf rpcProtection

instance Data.ToJSON QopConfiguration where
  toJSON QopConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataTransferProtection" Data..=)
              Prelude.<$> dataTransferProtection,
            ("RpcProtection" Data..=) Prelude.<$> rpcProtection
          ]
      )
