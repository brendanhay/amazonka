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
-- Module      : Amazonka.RDS.Types.GlobalClusterMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.GlobalClusterMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.WriteForwardingStatus

-- | A data structure with information about any primary and secondary
-- clusters associated with an Aurora global database.
--
-- /See:/ 'newGlobalClusterMember' smart constructor.
data GlobalClusterMember = GlobalClusterMember'
  { -- | The Amazon Resource Name (ARN) for each Aurora cluster.
    dbClusterArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a secondary cluster in an Aurora global database has
    -- write forwarding enabled, not enabled, or is in the process of enabling
    -- it.
    globalWriteForwardingStatus :: Prelude.Maybe WriteForwardingStatus,
    -- | Specifies whether the Aurora cluster is the primary cluster (that is,
    -- has read-write capability) for the Aurora global database with which it
    -- is associated.
    isWriter :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for each read-only secondary cluster
    -- associated with the Aurora global database.
    readers :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalClusterMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterArn', 'globalClusterMember_dbClusterArn' - The Amazon Resource Name (ARN) for each Aurora cluster.
--
-- 'globalWriteForwardingStatus', 'globalClusterMember_globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
--
-- 'isWriter', 'globalClusterMember_isWriter' - Specifies whether the Aurora cluster is the primary cluster (that is,
-- has read-write capability) for the Aurora global database with which it
-- is associated.
--
-- 'readers', 'globalClusterMember_readers' - The Amazon Resource Name (ARN) for each read-only secondary cluster
-- associated with the Aurora global database.
newGlobalClusterMember ::
  GlobalClusterMember
newGlobalClusterMember =
  GlobalClusterMember'
    { dbClusterArn =
        Prelude.Nothing,
      globalWriteForwardingStatus = Prelude.Nothing,
      isWriter = Prelude.Nothing,
      readers = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for each Aurora cluster.
globalClusterMember_dbClusterArn :: Lens.Lens' GlobalClusterMember (Prelude.Maybe Prelude.Text)
globalClusterMember_dbClusterArn = Lens.lens (\GlobalClusterMember' {dbClusterArn} -> dbClusterArn) (\s@GlobalClusterMember' {} a -> s {dbClusterArn = a} :: GlobalClusterMember)

-- | Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
globalClusterMember_globalWriteForwardingStatus :: Lens.Lens' GlobalClusterMember (Prelude.Maybe WriteForwardingStatus)
globalClusterMember_globalWriteForwardingStatus = Lens.lens (\GlobalClusterMember' {globalWriteForwardingStatus} -> globalWriteForwardingStatus) (\s@GlobalClusterMember' {} a -> s {globalWriteForwardingStatus = a} :: GlobalClusterMember)

-- | Specifies whether the Aurora cluster is the primary cluster (that is,
-- has read-write capability) for the Aurora global database with which it
-- is associated.
globalClusterMember_isWriter :: Lens.Lens' GlobalClusterMember (Prelude.Maybe Prelude.Bool)
globalClusterMember_isWriter = Lens.lens (\GlobalClusterMember' {isWriter} -> isWriter) (\s@GlobalClusterMember' {} a -> s {isWriter = a} :: GlobalClusterMember)

-- | The Amazon Resource Name (ARN) for each read-only secondary cluster
-- associated with the Aurora global database.
globalClusterMember_readers :: Lens.Lens' GlobalClusterMember (Prelude.Maybe [Prelude.Text])
globalClusterMember_readers = Lens.lens (\GlobalClusterMember' {readers} -> readers) (\s@GlobalClusterMember' {} a -> s {readers = a} :: GlobalClusterMember) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML GlobalClusterMember where
  parseXML x =
    GlobalClusterMember'
      Prelude.<$> (x Data..@? "DBClusterArn")
      Prelude.<*> (x Data..@? "GlobalWriteForwardingStatus")
      Prelude.<*> (x Data..@? "IsWriter")
      Prelude.<*> ( x Data..@? "Readers" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable GlobalClusterMember where
  hashWithSalt _salt GlobalClusterMember' {..} =
    _salt `Prelude.hashWithSalt` dbClusterArn
      `Prelude.hashWithSalt` globalWriteForwardingStatus
      `Prelude.hashWithSalt` isWriter
      `Prelude.hashWithSalt` readers

instance Prelude.NFData GlobalClusterMember where
  rnf GlobalClusterMember' {..} =
    Prelude.rnf dbClusterArn
      `Prelude.seq` Prelude.rnf globalWriteForwardingStatus
      `Prelude.seq` Prelude.rnf isWriter
      `Prelude.seq` Prelude.rnf readers
