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
-- Module      : Network.AWS.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns an assigned Amazon EBS volume. The volume remains registered
-- with the stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.UnassignVolume
  ( -- * Creating a Request
    UnassignVolume (..),
    newUnassignVolume,

    -- * Request Lenses
    unassignVolume_volumeId,

    -- * Destructuring the Response
    UnassignVolumeResponse (..),
    newUnassignVolumeResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnassignVolume' smart constructor.
data UnassignVolume = UnassignVolume'
  { -- | The volume ID.
    volumeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnassignVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeId', 'unassignVolume_volumeId' - The volume ID.
newUnassignVolume ::
  -- | 'volumeId'
  Core.Text ->
  UnassignVolume
newUnassignVolume pVolumeId_ =
  UnassignVolume' {volumeId = pVolumeId_}

-- | The volume ID.
unassignVolume_volumeId :: Lens.Lens' UnassignVolume Core.Text
unassignVolume_volumeId = Lens.lens (\UnassignVolume' {volumeId} -> volumeId) (\s@UnassignVolume' {} a -> s {volumeId = a} :: UnassignVolume)

instance Core.AWSRequest UnassignVolume where
  type
    AWSResponse UnassignVolume =
      UnassignVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UnassignVolumeResponse'

instance Core.Hashable UnassignVolume

instance Core.NFData UnassignVolume

instance Core.ToHeaders UnassignVolume where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.UnassignVolume" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UnassignVolume where
  toJSON UnassignVolume' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VolumeId" Core..= volumeId)]
      )

instance Core.ToPath UnassignVolume where
  toPath = Core.const "/"

instance Core.ToQuery UnassignVolume where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUnassignVolumeResponse' smart constructor.
data UnassignVolumeResponse = UnassignVolumeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnassignVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnassignVolumeResponse ::
  UnassignVolumeResponse
newUnassignVolumeResponse = UnassignVolumeResponse'

instance Core.NFData UnassignVolumeResponse
