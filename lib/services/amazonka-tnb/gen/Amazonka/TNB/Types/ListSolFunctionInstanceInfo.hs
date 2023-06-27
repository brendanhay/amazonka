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
-- Module      : Amazonka.TNB.Types.ListSolFunctionInstanceInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ListSolFunctionInstanceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.GetSolInstantiatedVnfInfo
import Amazonka.TNB.Types.ListSolFunctionInstanceMetadata
import Amazonka.TNB.Types.VnfInstantiationState

-- | Lists information about a network function instance.
--
-- A network function instance is a function in a function package .
--
-- /See:/ 'newListSolFunctionInstanceInfo' smart constructor.
data ListSolFunctionInstanceInfo = ListSolFunctionInstanceInfo'
  { instantiatedVnfInfo :: Prelude.Maybe GetSolInstantiatedVnfInfo,
    -- | Function package name.
    vnfPkgName :: Prelude.Maybe Prelude.Text,
    -- | Network function instance ARN.
    arn :: Prelude.Text,
    -- | Network function instance ID.
    id :: Prelude.Text,
    -- | Network function instance instantiation state.
    instantiationState :: VnfInstantiationState,
    -- | Network function instance metadata.
    metadata :: ListSolFunctionInstanceMetadata,
    -- | Network instance ID.
    nsInstanceId :: Prelude.Text,
    -- | Function package ID.
    vnfPkgId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionInstanceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instantiatedVnfInfo', 'listSolFunctionInstanceInfo_instantiatedVnfInfo' - Undocumented member.
--
-- 'vnfPkgName', 'listSolFunctionInstanceInfo_vnfPkgName' - Function package name.
--
-- 'arn', 'listSolFunctionInstanceInfo_arn' - Network function instance ARN.
--
-- 'id', 'listSolFunctionInstanceInfo_id' - Network function instance ID.
--
-- 'instantiationState', 'listSolFunctionInstanceInfo_instantiationState' - Network function instance instantiation state.
--
-- 'metadata', 'listSolFunctionInstanceInfo_metadata' - Network function instance metadata.
--
-- 'nsInstanceId', 'listSolFunctionInstanceInfo_nsInstanceId' - Network instance ID.
--
-- 'vnfPkgId', 'listSolFunctionInstanceInfo_vnfPkgId' - Function package ID.
newListSolFunctionInstanceInfo ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'instantiationState'
  VnfInstantiationState ->
  -- | 'metadata'
  ListSolFunctionInstanceMetadata ->
  -- | 'nsInstanceId'
  Prelude.Text ->
  -- | 'vnfPkgId'
  Prelude.Text ->
  ListSolFunctionInstanceInfo
newListSolFunctionInstanceInfo
  pArn_
  pId_
  pInstantiationState_
  pMetadata_
  pNsInstanceId_
  pVnfPkgId_ =
    ListSolFunctionInstanceInfo'
      { instantiatedVnfInfo =
          Prelude.Nothing,
        vnfPkgName = Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        instantiationState = pInstantiationState_,
        metadata = pMetadata_,
        nsInstanceId = pNsInstanceId_,
        vnfPkgId = pVnfPkgId_
      }

-- | Undocumented member.
listSolFunctionInstanceInfo_instantiatedVnfInfo :: Lens.Lens' ListSolFunctionInstanceInfo (Prelude.Maybe GetSolInstantiatedVnfInfo)
listSolFunctionInstanceInfo_instantiatedVnfInfo = Lens.lens (\ListSolFunctionInstanceInfo' {instantiatedVnfInfo} -> instantiatedVnfInfo) (\s@ListSolFunctionInstanceInfo' {} a -> s {instantiatedVnfInfo = a} :: ListSolFunctionInstanceInfo)

-- | Function package name.
listSolFunctionInstanceInfo_vnfPkgName :: Lens.Lens' ListSolFunctionInstanceInfo (Prelude.Maybe Prelude.Text)
listSolFunctionInstanceInfo_vnfPkgName = Lens.lens (\ListSolFunctionInstanceInfo' {vnfPkgName} -> vnfPkgName) (\s@ListSolFunctionInstanceInfo' {} a -> s {vnfPkgName = a} :: ListSolFunctionInstanceInfo)

-- | Network function instance ARN.
listSolFunctionInstanceInfo_arn :: Lens.Lens' ListSolFunctionInstanceInfo Prelude.Text
listSolFunctionInstanceInfo_arn = Lens.lens (\ListSolFunctionInstanceInfo' {arn} -> arn) (\s@ListSolFunctionInstanceInfo' {} a -> s {arn = a} :: ListSolFunctionInstanceInfo)

-- | Network function instance ID.
listSolFunctionInstanceInfo_id :: Lens.Lens' ListSolFunctionInstanceInfo Prelude.Text
listSolFunctionInstanceInfo_id = Lens.lens (\ListSolFunctionInstanceInfo' {id} -> id) (\s@ListSolFunctionInstanceInfo' {} a -> s {id = a} :: ListSolFunctionInstanceInfo)

-- | Network function instance instantiation state.
listSolFunctionInstanceInfo_instantiationState :: Lens.Lens' ListSolFunctionInstanceInfo VnfInstantiationState
listSolFunctionInstanceInfo_instantiationState = Lens.lens (\ListSolFunctionInstanceInfo' {instantiationState} -> instantiationState) (\s@ListSolFunctionInstanceInfo' {} a -> s {instantiationState = a} :: ListSolFunctionInstanceInfo)

-- | Network function instance metadata.
listSolFunctionInstanceInfo_metadata :: Lens.Lens' ListSolFunctionInstanceInfo ListSolFunctionInstanceMetadata
listSolFunctionInstanceInfo_metadata = Lens.lens (\ListSolFunctionInstanceInfo' {metadata} -> metadata) (\s@ListSolFunctionInstanceInfo' {} a -> s {metadata = a} :: ListSolFunctionInstanceInfo)

-- | Network instance ID.
listSolFunctionInstanceInfo_nsInstanceId :: Lens.Lens' ListSolFunctionInstanceInfo Prelude.Text
listSolFunctionInstanceInfo_nsInstanceId = Lens.lens (\ListSolFunctionInstanceInfo' {nsInstanceId} -> nsInstanceId) (\s@ListSolFunctionInstanceInfo' {} a -> s {nsInstanceId = a} :: ListSolFunctionInstanceInfo)

-- | Function package ID.
listSolFunctionInstanceInfo_vnfPkgId :: Lens.Lens' ListSolFunctionInstanceInfo Prelude.Text
listSolFunctionInstanceInfo_vnfPkgId = Lens.lens (\ListSolFunctionInstanceInfo' {vnfPkgId} -> vnfPkgId) (\s@ListSolFunctionInstanceInfo' {} a -> s {vnfPkgId = a} :: ListSolFunctionInstanceInfo)

instance Data.FromJSON ListSolFunctionInstanceInfo where
  parseJSON =
    Data.withObject
      "ListSolFunctionInstanceInfo"
      ( \x ->
          ListSolFunctionInstanceInfo'
            Prelude.<$> (x Data..:? "instantiatedVnfInfo")
            Prelude.<*> (x Data..:? "vnfPkgName")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "instantiationState")
            Prelude.<*> (x Data..: "metadata")
            Prelude.<*> (x Data..: "nsInstanceId")
            Prelude.<*> (x Data..: "vnfPkgId")
      )

instance Prelude.Hashable ListSolFunctionInstanceInfo where
  hashWithSalt _salt ListSolFunctionInstanceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` instantiatedVnfInfo
      `Prelude.hashWithSalt` vnfPkgName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instantiationState
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` nsInstanceId
      `Prelude.hashWithSalt` vnfPkgId

instance Prelude.NFData ListSolFunctionInstanceInfo where
  rnf ListSolFunctionInstanceInfo' {..} =
    Prelude.rnf instantiatedVnfInfo
      `Prelude.seq` Prelude.rnf vnfPkgName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instantiationState
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsInstanceId
      `Prelude.seq` Prelude.rnf vnfPkgId
