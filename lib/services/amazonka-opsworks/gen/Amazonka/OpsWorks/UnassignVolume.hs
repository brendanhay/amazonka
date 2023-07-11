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
-- Module      : Amazonka.OpsWorks.UnassignVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.OpsWorks.UnassignVolume
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnassignVolume' smart constructor.
data UnassignVolume = UnassignVolume'
  { -- | The volume ID.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UnassignVolume
newUnassignVolume pVolumeId_ =
  UnassignVolume' {volumeId = pVolumeId_}

-- | The volume ID.
unassignVolume_volumeId :: Lens.Lens' UnassignVolume Prelude.Text
unassignVolume_volumeId = Lens.lens (\UnassignVolume' {volumeId} -> volumeId) (\s@UnassignVolume' {} a -> s {volumeId = a} :: UnassignVolume)

instance Core.AWSRequest UnassignVolume where
  type
    AWSResponse UnassignVolume =
      UnassignVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UnassignVolumeResponse'

instance Prelude.Hashable UnassignVolume where
  hashWithSalt _salt UnassignVolume' {..} =
    _salt `Prelude.hashWithSalt` volumeId

instance Prelude.NFData UnassignVolume where
  rnf UnassignVolume' {..} = Prelude.rnf volumeId

instance Data.ToHeaders UnassignVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UnassignVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnassignVolume where
  toJSON UnassignVolume' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeId" Data..= volumeId)]
      )

instance Data.ToPath UnassignVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery UnassignVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnassignVolumeResponse' smart constructor.
data UnassignVolumeResponse = UnassignVolumeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnassignVolumeResponse ::
  UnassignVolumeResponse
newUnassignVolumeResponse = UnassignVolumeResponse'

instance Prelude.NFData UnassignVolumeResponse where
  rnf _ = ()
