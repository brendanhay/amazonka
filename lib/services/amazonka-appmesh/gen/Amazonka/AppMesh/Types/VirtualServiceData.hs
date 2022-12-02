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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceData where

import Amazonka.AppMesh.Types.ResourceMetadata
import Amazonka.AppMesh.Types.VirtualServiceSpec
import Amazonka.AppMesh.Types.VirtualServiceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual service returned by a describe
-- operation.
--
-- /See:/ 'newVirtualServiceData' smart constructor.
data VirtualServiceData = VirtualServiceData'
  { -- | The name of the service mesh that the virtual service resides in.
    meshName :: Prelude.Text,
    metadata :: ResourceMetadata,
    -- | The specifications of the virtual service.
    spec :: VirtualServiceSpec,
    -- | The current status of the virtual service.
    status :: VirtualServiceStatus,
    -- | The name of the virtual service.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualServiceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshName', 'virtualServiceData_meshName' - The name of the service mesh that the virtual service resides in.
--
-- 'metadata', 'virtualServiceData_metadata' - Undocumented member.
--
-- 'spec', 'virtualServiceData_spec' - The specifications of the virtual service.
--
-- 'status', 'virtualServiceData_status' - The current status of the virtual service.
--
-- 'virtualServiceName', 'virtualServiceData_virtualServiceName' - The name of the virtual service.
newVirtualServiceData ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'metadata'
  ResourceMetadata ->
  -- | 'spec'
  VirtualServiceSpec ->
  -- | 'status'
  VirtualServiceStatus ->
  -- | 'virtualServiceName'
  Prelude.Text ->
  VirtualServiceData
newVirtualServiceData
  pMeshName_
  pMetadata_
  pSpec_
  pStatus_
  pVirtualServiceName_ =
    VirtualServiceData'
      { meshName = pMeshName_,
        metadata = pMetadata_,
        spec = pSpec_,
        status = pStatus_,
        virtualServiceName = pVirtualServiceName_
      }

-- | The name of the service mesh that the virtual service resides in.
virtualServiceData_meshName :: Lens.Lens' VirtualServiceData Prelude.Text
virtualServiceData_meshName = Lens.lens (\VirtualServiceData' {meshName} -> meshName) (\s@VirtualServiceData' {} a -> s {meshName = a} :: VirtualServiceData)

-- | Undocumented member.
virtualServiceData_metadata :: Lens.Lens' VirtualServiceData ResourceMetadata
virtualServiceData_metadata = Lens.lens (\VirtualServiceData' {metadata} -> metadata) (\s@VirtualServiceData' {} a -> s {metadata = a} :: VirtualServiceData)

-- | The specifications of the virtual service.
virtualServiceData_spec :: Lens.Lens' VirtualServiceData VirtualServiceSpec
virtualServiceData_spec = Lens.lens (\VirtualServiceData' {spec} -> spec) (\s@VirtualServiceData' {} a -> s {spec = a} :: VirtualServiceData)

-- | The current status of the virtual service.
virtualServiceData_status :: Lens.Lens' VirtualServiceData VirtualServiceStatus
virtualServiceData_status = Lens.lens (\VirtualServiceData' {status} -> status) (\s@VirtualServiceData' {} a -> s {status = a} :: VirtualServiceData)

-- | The name of the virtual service.
virtualServiceData_virtualServiceName :: Lens.Lens' VirtualServiceData Prelude.Text
virtualServiceData_virtualServiceName = Lens.lens (\VirtualServiceData' {virtualServiceName} -> virtualServiceName) (\s@VirtualServiceData' {} a -> s {virtualServiceName = a} :: VirtualServiceData)

instance Data.FromJSON VirtualServiceData where
  parseJSON =
    Data.withObject
      "VirtualServiceData"
      ( \x ->
          VirtualServiceData'
            Prelude.<$> (x Data..: "meshName")
            Prelude.<*> (x Data..: "metadata")
            Prelude.<*> (x Data..: "spec")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "virtualServiceName")
      )

instance Prelude.Hashable VirtualServiceData where
  hashWithSalt _salt VirtualServiceData' {..} =
    _salt `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData VirtualServiceData where
  rnf VirtualServiceData' {..} =
    Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf virtualServiceName
