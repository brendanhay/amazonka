{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.ModifyVolumeAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a volume attribute.
--
-- By default, all I\/O operations for the volume are suspended when the
-- data on the volume is determined to be potentially inconsistent, to
-- prevent undetectable, latent data corruption. The I\/O access to the
-- volume can be resumed by first enabling I\/O access and then checking
-- the data consistency on your volume.
--
-- You can change the default behavior to resume I\/O operations. We
-- recommend that you change this only for boot volumes or for volumes that
-- are stateless or disposable.
module Amazonka.EC2.ModifyVolumeAttribute
  ( -- * Creating a Request
    ModifyVolumeAttribute (..),
    newModifyVolumeAttribute,

    -- * Request Lenses
    modifyVolumeAttribute_autoEnableIO,
    modifyVolumeAttribute_dryRun,
    modifyVolumeAttribute_volumeId,

    -- * Destructuring the Response
    ModifyVolumeAttributeResponse (..),
    newModifyVolumeAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVolumeAttribute' smart constructor.
data ModifyVolumeAttribute = ModifyVolumeAttribute'
  { -- | Indicates whether the volume should be auto-enabled for I\/O operations.
    autoEnableIO :: Prelude.Maybe AttributeBooleanValue,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVolumeAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnableIO', 'modifyVolumeAttribute_autoEnableIO' - Indicates whether the volume should be auto-enabled for I\/O operations.
--
-- 'dryRun', 'modifyVolumeAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'volumeId', 'modifyVolumeAttribute_volumeId' - The ID of the volume.
newModifyVolumeAttribute ::
  -- | 'volumeId'
  Prelude.Text ->
  ModifyVolumeAttribute
newModifyVolumeAttribute pVolumeId_ =
  ModifyVolumeAttribute'
    { autoEnableIO =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | Indicates whether the volume should be auto-enabled for I\/O operations.
modifyVolumeAttribute_autoEnableIO :: Lens.Lens' ModifyVolumeAttribute (Prelude.Maybe AttributeBooleanValue)
modifyVolumeAttribute_autoEnableIO = Lens.lens (\ModifyVolumeAttribute' {autoEnableIO} -> autoEnableIO) (\s@ModifyVolumeAttribute' {} a -> s {autoEnableIO = a} :: ModifyVolumeAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVolumeAttribute_dryRun :: Lens.Lens' ModifyVolumeAttribute (Prelude.Maybe Prelude.Bool)
modifyVolumeAttribute_dryRun = Lens.lens (\ModifyVolumeAttribute' {dryRun} -> dryRun) (\s@ModifyVolumeAttribute' {} a -> s {dryRun = a} :: ModifyVolumeAttribute)

-- | The ID of the volume.
modifyVolumeAttribute_volumeId :: Lens.Lens' ModifyVolumeAttribute Prelude.Text
modifyVolumeAttribute_volumeId = Lens.lens (\ModifyVolumeAttribute' {volumeId} -> volumeId) (\s@ModifyVolumeAttribute' {} a -> s {volumeId = a} :: ModifyVolumeAttribute)

instance Core.AWSRequest ModifyVolumeAttribute where
  type
    AWSResponse ModifyVolumeAttribute =
      ModifyVolumeAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ModifyVolumeAttributeResponse'

instance Prelude.Hashable ModifyVolumeAttribute where
  hashWithSalt _salt ModifyVolumeAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` autoEnableIO
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData ModifyVolumeAttribute where
  rnf ModifyVolumeAttribute' {..} =
    Prelude.rnf autoEnableIO
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf volumeId

instance Data.ToHeaders ModifyVolumeAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVolumeAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVolumeAttribute where
  toQuery ModifyVolumeAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVolumeAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AutoEnableIO" Data.=: autoEnableIO,
        "DryRun" Data.=: dryRun,
        "VolumeId" Data.=: volumeId
      ]

-- | /See:/ 'newModifyVolumeAttributeResponse' smart constructor.
data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVolumeAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyVolumeAttributeResponse ::
  ModifyVolumeAttributeResponse
newModifyVolumeAttributeResponse =
  ModifyVolumeAttributeResponse'

instance Prelude.NFData ModifyVolumeAttributeResponse where
  rnf _ = ()
