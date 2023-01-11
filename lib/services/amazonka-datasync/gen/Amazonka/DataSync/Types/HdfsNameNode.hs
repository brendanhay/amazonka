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
-- Module      : Amazonka.DataSync.Types.HdfsNameNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.HdfsNameNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The NameNode of the Hadoop Distributed File System (HDFS). The NameNode
-- manages the file system\'s namespace. The NameNode performs operations
-- such as opening, closing, and renaming files and directories. The
-- NameNode contains the information to map blocks of data to the
-- DataNodes.
--
-- /See:/ 'newHdfsNameNode' smart constructor.
data HdfsNameNode = HdfsNameNode'
  { -- | The hostname of the NameNode in the HDFS cluster. This value is the IP
    -- address or Domain Name Service (DNS) name of the NameNode. An agent
    -- that\'s installed on-premises uses this hostname to communicate with the
    -- NameNode in the network.
    hostname :: Prelude.Text,
    -- | The port that the NameNode uses to listen to client requests.
    port :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HdfsNameNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'hdfsNameNode_hostname' - The hostname of the NameNode in the HDFS cluster. This value is the IP
-- address or Domain Name Service (DNS) name of the NameNode. An agent
-- that\'s installed on-premises uses this hostname to communicate with the
-- NameNode in the network.
--
-- 'port', 'hdfsNameNode_port' - The port that the NameNode uses to listen to client requests.
newHdfsNameNode ::
  -- | 'hostname'
  Prelude.Text ->
  -- | 'port'
  Prelude.Natural ->
  HdfsNameNode
newHdfsNameNode pHostname_ pPort_ =
  HdfsNameNode' {hostname = pHostname_, port = pPort_}

-- | The hostname of the NameNode in the HDFS cluster. This value is the IP
-- address or Domain Name Service (DNS) name of the NameNode. An agent
-- that\'s installed on-premises uses this hostname to communicate with the
-- NameNode in the network.
hdfsNameNode_hostname :: Lens.Lens' HdfsNameNode Prelude.Text
hdfsNameNode_hostname = Lens.lens (\HdfsNameNode' {hostname} -> hostname) (\s@HdfsNameNode' {} a -> s {hostname = a} :: HdfsNameNode)

-- | The port that the NameNode uses to listen to client requests.
hdfsNameNode_port :: Lens.Lens' HdfsNameNode Prelude.Natural
hdfsNameNode_port = Lens.lens (\HdfsNameNode' {port} -> port) (\s@HdfsNameNode' {} a -> s {port = a} :: HdfsNameNode)

instance Data.FromJSON HdfsNameNode where
  parseJSON =
    Data.withObject
      "HdfsNameNode"
      ( \x ->
          HdfsNameNode'
            Prelude.<$> (x Data..: "Hostname")
            Prelude.<*> (x Data..: "Port")
      )

instance Prelude.Hashable HdfsNameNode where
  hashWithSalt _salt HdfsNameNode' {..} =
    _salt `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` port

instance Prelude.NFData HdfsNameNode where
  rnf HdfsNameNode' {..} =
    Prelude.rnf hostname `Prelude.seq` Prelude.rnf port

instance Data.ToJSON HdfsNameNode where
  toJSON HdfsNameNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Hostname" Data..= hostname),
            Prelude.Just ("Port" Data..= port)
          ]
      )
