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
-- Module      : Amazonka.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a network interface from an instance.
module Amazonka.EC2.DetachNetworkInterface
  ( -- * Creating a Request
    DetachNetworkInterface (..),
    newDetachNetworkInterface,

    -- * Request Lenses
    detachNetworkInterface_dryRun,
    detachNetworkInterface_force,
    detachNetworkInterface_attachmentId,

    -- * Destructuring the Response
    DetachNetworkInterfaceResponse (..),
    newDetachNetworkInterfaceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DetachNetworkInterface.
--
-- /See:/ 'newDetachNetworkInterface' smart constructor.
data DetachNetworkInterface = DetachNetworkInterface'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to force a detachment.
    --
    -- -   Use the @Force@ parameter only as a last resort to detach a network
    --     interface from a failed instance.
    --
    -- -   If you use the @Force@ parameter to detach a network interface, you
    --     might not be able to attach a different network interface to the
    --     same index on the instance without first stopping and starting the
    --     instance.
    --
    -- -   If you force the detachment of a network interface, the
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html instance metadata>
    --     might not get updated. This means that the attributes associated
    --     with the detached network interface might still be visible. The
    --     instance metadata will get updated when you stop and start the
    --     instance.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'detachNetworkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'force', 'detachNetworkInterface_force' - Specifies whether to force a detachment.
--
-- -   Use the @Force@ parameter only as a last resort to detach a network
--     interface from a failed instance.
--
-- -   If you use the @Force@ parameter to detach a network interface, you
--     might not be able to attach a different network interface to the
--     same index on the instance without first stopping and starting the
--     instance.
--
-- -   If you force the detachment of a network interface, the
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html instance metadata>
--     might not get updated. This means that the attributes associated
--     with the detached network interface might still be visible. The
--     instance metadata will get updated when you stop and start the
--     instance.
--
-- 'attachmentId', 'detachNetworkInterface_attachmentId' - The ID of the attachment.
newDetachNetworkInterface ::
  -- | 'attachmentId'
  Prelude.Text ->
  DetachNetworkInterface
newDetachNetworkInterface pAttachmentId_ =
  DetachNetworkInterface'
    { dryRun = Prelude.Nothing,
      force = Prelude.Nothing,
      attachmentId = pAttachmentId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachNetworkInterface_dryRun :: Lens.Lens' DetachNetworkInterface (Prelude.Maybe Prelude.Bool)
detachNetworkInterface_dryRun = Lens.lens (\DetachNetworkInterface' {dryRun} -> dryRun) (\s@DetachNetworkInterface' {} a -> s {dryRun = a} :: DetachNetworkInterface)

-- | Specifies whether to force a detachment.
--
-- -   Use the @Force@ parameter only as a last resort to detach a network
--     interface from a failed instance.
--
-- -   If you use the @Force@ parameter to detach a network interface, you
--     might not be able to attach a different network interface to the
--     same index on the instance without first stopping and starting the
--     instance.
--
-- -   If you force the detachment of a network interface, the
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html instance metadata>
--     might not get updated. This means that the attributes associated
--     with the detached network interface might still be visible. The
--     instance metadata will get updated when you stop and start the
--     instance.
detachNetworkInterface_force :: Lens.Lens' DetachNetworkInterface (Prelude.Maybe Prelude.Bool)
detachNetworkInterface_force = Lens.lens (\DetachNetworkInterface' {force} -> force) (\s@DetachNetworkInterface' {} a -> s {force = a} :: DetachNetworkInterface)

-- | The ID of the attachment.
detachNetworkInterface_attachmentId :: Lens.Lens' DetachNetworkInterface Prelude.Text
detachNetworkInterface_attachmentId = Lens.lens (\DetachNetworkInterface' {attachmentId} -> attachmentId) (\s@DetachNetworkInterface' {} a -> s {attachmentId = a} :: DetachNetworkInterface)

instance Core.AWSRequest DetachNetworkInterface where
  type
    AWSResponse DetachNetworkInterface =
      DetachNetworkInterfaceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DetachNetworkInterfaceResponse'

instance Prelude.Hashable DetachNetworkInterface where
  hashWithSalt _salt DetachNetworkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData DetachNetworkInterface where
  rnf DetachNetworkInterface' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf attachmentId

instance Data.ToHeaders DetachNetworkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachNetworkInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachNetworkInterface where
  toQuery DetachNetworkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DetachNetworkInterface" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Force" Data.=: force,
        "AttachmentId" Data.=: attachmentId
      ]

-- | /See:/ 'newDetachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachNetworkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachNetworkInterfaceResponse ::
  DetachNetworkInterfaceResponse
newDetachNetworkInterfaceResponse =
  DetachNetworkInterfaceResponse'

instance
  Prelude.NFData
    DetachNetworkInterfaceResponse
  where
  rnf _ = ()
