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
-- Module      : Network.AWS.RDS.Types.GlobalClusterMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.GlobalClusterMember where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.WriteForwardingStatus

-- | A data structure with information about any primary and secondary
-- clusters associated with an Aurora global database.
--
-- /See:/ 'newGlobalClusterMember' smart constructor.
data GlobalClusterMember = GlobalClusterMember'
  { -- | Specifies whether a secondary cluster in an Aurora global database has
    -- write forwarding enabled, not enabled, or is in the process of enabling
    -- it.
    globalWriteForwardingStatus :: Prelude.Maybe WriteForwardingStatus,
    -- | The Amazon Resource Name (ARN) for each Aurora cluster.
    dbClusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for each read-only secondary cluster
    -- associated with the Aurora global database.
    readers :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the Aurora cluster is the primary cluster (that is,
    -- has read-write capability) for the Aurora global database with which it
    -- is associated.
    isWriter :: Prelude.Maybe Prelude.Bool
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
-- 'globalWriteForwardingStatus', 'globalClusterMember_globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
--
-- 'dbClusterArn', 'globalClusterMember_dbClusterArn' - The Amazon Resource Name (ARN) for each Aurora cluster.
--
-- 'readers', 'globalClusterMember_readers' - The Amazon Resource Name (ARN) for each read-only secondary cluster
-- associated with the Aurora global database.
--
-- 'isWriter', 'globalClusterMember_isWriter' - Specifies whether the Aurora cluster is the primary cluster (that is,
-- has read-write capability) for the Aurora global database with which it
-- is associated.
newGlobalClusterMember ::
  GlobalClusterMember
newGlobalClusterMember =
  GlobalClusterMember'
    { globalWriteForwardingStatus =
        Prelude.Nothing,
      dbClusterArn = Prelude.Nothing,
      readers = Prelude.Nothing,
      isWriter = Prelude.Nothing
    }

-- | Specifies whether a secondary cluster in an Aurora global database has
-- write forwarding enabled, not enabled, or is in the process of enabling
-- it.
globalClusterMember_globalWriteForwardingStatus :: Lens.Lens' GlobalClusterMember (Prelude.Maybe WriteForwardingStatus)
globalClusterMember_globalWriteForwardingStatus = Lens.lens (\GlobalClusterMember' {globalWriteForwardingStatus} -> globalWriteForwardingStatus) (\s@GlobalClusterMember' {} a -> s {globalWriteForwardingStatus = a} :: GlobalClusterMember)

-- | The Amazon Resource Name (ARN) for each Aurora cluster.
globalClusterMember_dbClusterArn :: Lens.Lens' GlobalClusterMember (Prelude.Maybe Prelude.Text)
globalClusterMember_dbClusterArn = Lens.lens (\GlobalClusterMember' {dbClusterArn} -> dbClusterArn) (\s@GlobalClusterMember' {} a -> s {dbClusterArn = a} :: GlobalClusterMember)

-- | The Amazon Resource Name (ARN) for each read-only secondary cluster
-- associated with the Aurora global database.
globalClusterMember_readers :: Lens.Lens' GlobalClusterMember (Prelude.Maybe [Prelude.Text])
globalClusterMember_readers = Lens.lens (\GlobalClusterMember' {readers} -> readers) (\s@GlobalClusterMember' {} a -> s {readers = a} :: GlobalClusterMember) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether the Aurora cluster is the primary cluster (that is,
-- has read-write capability) for the Aurora global database with which it
-- is associated.
globalClusterMember_isWriter :: Lens.Lens' GlobalClusterMember (Prelude.Maybe Prelude.Bool)
globalClusterMember_isWriter = Lens.lens (\GlobalClusterMember' {isWriter} -> isWriter) (\s@GlobalClusterMember' {} a -> s {isWriter = a} :: GlobalClusterMember)

instance Core.FromXML GlobalClusterMember where
  parseXML x =
    GlobalClusterMember'
      Prelude.<$> (x Core..@? "GlobalWriteForwardingStatus")
      Prelude.<*> (x Core..@? "DBClusterArn")
      Prelude.<*> ( x Core..@? "Readers" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "IsWriter")

instance Prelude.Hashable GlobalClusterMember

instance Prelude.NFData GlobalClusterMember
