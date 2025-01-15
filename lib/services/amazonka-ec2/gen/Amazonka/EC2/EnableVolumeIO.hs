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
-- Module      : Amazonka.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables I\/O operations for a volume that had I\/O operations disabled
-- because the data on the volume was potentially inconsistent.
module Amazonka.EC2.EnableVolumeIO
  ( -- * Creating a Request
    EnableVolumeIO (..),
    newEnableVolumeIO,

    -- * Request Lenses
    enableVolumeIO_dryRun,
    enableVolumeIO_volumeId,

    -- * Destructuring the Response
    EnableVolumeIOResponse (..),
    newEnableVolumeIOResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableVolumeIO' smart constructor.
data EnableVolumeIO = EnableVolumeIO'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVolumeIO' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableVolumeIO_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'volumeId', 'enableVolumeIO_volumeId' - The ID of the volume.
newEnableVolumeIO ::
  -- | 'volumeId'
  Prelude.Text ->
  EnableVolumeIO
newEnableVolumeIO pVolumeId_ =
  EnableVolumeIO'
    { dryRun = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableVolumeIO_dryRun :: Lens.Lens' EnableVolumeIO (Prelude.Maybe Prelude.Bool)
enableVolumeIO_dryRun = Lens.lens (\EnableVolumeIO' {dryRun} -> dryRun) (\s@EnableVolumeIO' {} a -> s {dryRun = a} :: EnableVolumeIO)

-- | The ID of the volume.
enableVolumeIO_volumeId :: Lens.Lens' EnableVolumeIO Prelude.Text
enableVolumeIO_volumeId = Lens.lens (\EnableVolumeIO' {volumeId} -> volumeId) (\s@EnableVolumeIO' {} a -> s {volumeId = a} :: EnableVolumeIO)

instance Core.AWSRequest EnableVolumeIO where
  type
    AWSResponse EnableVolumeIO =
      EnableVolumeIOResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull EnableVolumeIOResponse'

instance Prelude.Hashable EnableVolumeIO where
  hashWithSalt _salt EnableVolumeIO' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData EnableVolumeIO where
  rnf EnableVolumeIO' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf volumeId

instance Data.ToHeaders EnableVolumeIO where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableVolumeIO where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableVolumeIO where
  toQuery EnableVolumeIO' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableVolumeIO" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VolumeId" Data.=: volumeId
      ]

-- | /See:/ 'newEnableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse = EnableVolumeIOResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableVolumeIOResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableVolumeIOResponse ::
  EnableVolumeIOResponse
newEnableVolumeIOResponse = EnableVolumeIOResponse'

instance Prelude.NFData EnableVolumeIOResponse where
  rnf _ = ()
