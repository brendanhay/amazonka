{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables I\/O operations for a volume that had I\/O operations disabled
-- because the data on the volume was potentially inconsistent.
module Network.AWS.EC2.EnableVolumeIO
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest EnableVolumeIO where
  type Rs EnableVolumeIO = EnableVolumeIOResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull EnableVolumeIOResponse'

instance Prelude.Hashable EnableVolumeIO

instance Prelude.NFData EnableVolumeIO

instance Prelude.ToHeaders EnableVolumeIO where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath EnableVolumeIO where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableVolumeIO where
  toQuery EnableVolumeIO' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("EnableVolumeIO" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "VolumeId" Prelude.=: volumeId
      ]

-- | /See:/ 'newEnableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse = EnableVolumeIOResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableVolumeIOResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableVolumeIOResponse ::
  EnableVolumeIOResponse
newEnableVolumeIOResponse = EnableVolumeIOResponse'

instance Prelude.NFData EnableVolumeIOResponse
