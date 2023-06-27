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
-- Module      : Amazonka.TNB.Types.ListSolNetworkInstanceInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolNetworkInstanceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.ListSolNetworkInstanceMetadata
import Amazonka.TNB.Types.NsState

-- | Info about the specific network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- /See:/ 'newListSolNetworkInstanceInfo' smart constructor.
data ListSolNetworkInstanceInfo = ListSolNetworkInstanceInfo'
  { -- | Network instance ARN.
    arn :: Prelude.Text,
    -- | ID of the network instance.
    id :: Prelude.Text,
    -- | The metadata of the network instance.
    metadata :: ListSolNetworkInstanceMetadata,
    -- | Human-readable description of the network instance.
    nsInstanceDescription :: Prelude.Text,
    -- | Human-readable name of the network instance.
    nsInstanceName :: Prelude.Text,
    -- | The state of the network instance.
    nsState :: NsState,
    -- | ID of the network service descriptor in the network package.
    nsdId :: Prelude.Text,
    -- | ID of the network service descriptor in the network package.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkInstanceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listSolNetworkInstanceInfo_arn' - Network instance ARN.
--
-- 'id', 'listSolNetworkInstanceInfo_id' - ID of the network instance.
--
-- 'metadata', 'listSolNetworkInstanceInfo_metadata' - The metadata of the network instance.
--
-- 'nsInstanceDescription', 'listSolNetworkInstanceInfo_nsInstanceDescription' - Human-readable description of the network instance.
--
-- 'nsInstanceName', 'listSolNetworkInstanceInfo_nsInstanceName' - Human-readable name of the network instance.
--
-- 'nsState', 'listSolNetworkInstanceInfo_nsState' - The state of the network instance.
--
-- 'nsdId', 'listSolNetworkInstanceInfo_nsdId' - ID of the network service descriptor in the network package.
--
-- 'nsdInfoId', 'listSolNetworkInstanceInfo_nsdInfoId' - ID of the network service descriptor in the network package.
newListSolNetworkInstanceInfo ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metadata'
  ListSolNetworkInstanceMetadata ->
  -- | 'nsInstanceDescription'
  Prelude.Text ->
  -- | 'nsInstanceName'
  Prelude.Text ->
  -- | 'nsState'
  NsState ->
  -- | 'nsdId'
  Prelude.Text ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  ListSolNetworkInstanceInfo
newListSolNetworkInstanceInfo
  pArn_
  pId_
  pMetadata_
  pNsInstanceDescription_
  pNsInstanceName_
  pNsState_
  pNsdId_
  pNsdInfoId_ =
    ListSolNetworkInstanceInfo'
      { arn = pArn_,
        id = pId_,
        metadata = pMetadata_,
        nsInstanceDescription = pNsInstanceDescription_,
        nsInstanceName = pNsInstanceName_,
        nsState = pNsState_,
        nsdId = pNsdId_,
        nsdInfoId = pNsdInfoId_
      }

-- | Network instance ARN.
listSolNetworkInstanceInfo_arn :: Lens.Lens' ListSolNetworkInstanceInfo Prelude.Text
listSolNetworkInstanceInfo_arn = Lens.lens (\ListSolNetworkInstanceInfo' {arn} -> arn) (\s@ListSolNetworkInstanceInfo' {} a -> s {arn = a} :: ListSolNetworkInstanceInfo)

-- | ID of the network instance.
listSolNetworkInstanceInfo_id :: Lens.Lens' ListSolNetworkInstanceInfo Prelude.Text
listSolNetworkInstanceInfo_id = Lens.lens (\ListSolNetworkInstanceInfo' {id} -> id) (\s@ListSolNetworkInstanceInfo' {} a -> s {id = a} :: ListSolNetworkInstanceInfo)

-- | The metadata of the network instance.
listSolNetworkInstanceInfo_metadata :: Lens.Lens' ListSolNetworkInstanceInfo ListSolNetworkInstanceMetadata
listSolNetworkInstanceInfo_metadata = Lens.lens (\ListSolNetworkInstanceInfo' {metadata} -> metadata) (\s@ListSolNetworkInstanceInfo' {} a -> s {metadata = a} :: ListSolNetworkInstanceInfo)

-- | Human-readable description of the network instance.
listSolNetworkInstanceInfo_nsInstanceDescription :: Lens.Lens' ListSolNetworkInstanceInfo Prelude.Text
listSolNetworkInstanceInfo_nsInstanceDescription = Lens.lens (\ListSolNetworkInstanceInfo' {nsInstanceDescription} -> nsInstanceDescription) (\s@ListSolNetworkInstanceInfo' {} a -> s {nsInstanceDescription = a} :: ListSolNetworkInstanceInfo)

-- | Human-readable name of the network instance.
listSolNetworkInstanceInfo_nsInstanceName :: Lens.Lens' ListSolNetworkInstanceInfo Prelude.Text
listSolNetworkInstanceInfo_nsInstanceName = Lens.lens (\ListSolNetworkInstanceInfo' {nsInstanceName} -> nsInstanceName) (\s@ListSolNetworkInstanceInfo' {} a -> s {nsInstanceName = a} :: ListSolNetworkInstanceInfo)

-- | The state of the network instance.
listSolNetworkInstanceInfo_nsState :: Lens.Lens' ListSolNetworkInstanceInfo NsState
listSolNetworkInstanceInfo_nsState = Lens.lens (\ListSolNetworkInstanceInfo' {nsState} -> nsState) (\s@ListSolNetworkInstanceInfo' {} a -> s {nsState = a} :: ListSolNetworkInstanceInfo)

-- | ID of the network service descriptor in the network package.
listSolNetworkInstanceInfo_nsdId :: Lens.Lens' ListSolNetworkInstanceInfo Prelude.Text
listSolNetworkInstanceInfo_nsdId = Lens.lens (\ListSolNetworkInstanceInfo' {nsdId} -> nsdId) (\s@ListSolNetworkInstanceInfo' {} a -> s {nsdId = a} :: ListSolNetworkInstanceInfo)

-- | ID of the network service descriptor in the network package.
listSolNetworkInstanceInfo_nsdInfoId :: Lens.Lens' ListSolNetworkInstanceInfo Prelude.Text
listSolNetworkInstanceInfo_nsdInfoId = Lens.lens (\ListSolNetworkInstanceInfo' {nsdInfoId} -> nsdInfoId) (\s@ListSolNetworkInstanceInfo' {} a -> s {nsdInfoId = a} :: ListSolNetworkInstanceInfo)

instance Data.FromJSON ListSolNetworkInstanceInfo where
  parseJSON =
    Data.withObject
      "ListSolNetworkInstanceInfo"
      ( \x ->
          ListSolNetworkInstanceInfo'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "metadata")
            Prelude.<*> (x Data..: "nsInstanceDescription")
            Prelude.<*> (x Data..: "nsInstanceName")
            Prelude.<*> (x Data..: "nsState")
            Prelude.<*> (x Data..: "nsdId")
            Prelude.<*> (x Data..: "nsdInfoId")
      )

instance Prelude.Hashable ListSolNetworkInstanceInfo where
  hashWithSalt _salt ListSolNetworkInstanceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` nsInstanceDescription
      `Prelude.hashWithSalt` nsInstanceName
      `Prelude.hashWithSalt` nsState
      `Prelude.hashWithSalt` nsdId
      `Prelude.hashWithSalt` nsdInfoId

instance Prelude.NFData ListSolNetworkInstanceInfo where
  rnf ListSolNetworkInstanceInfo' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsInstanceDescription
      `Prelude.seq` Prelude.rnf nsInstanceName
      `Prelude.seq` Prelude.rnf nsState
      `Prelude.seq` Prelude.rnf nsdId
      `Prelude.seq` Prelude.rnf nsdInfoId
