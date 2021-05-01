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
-- Module      : Network.AWS.EC2.DetachNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a network interface from an instance.
module Network.AWS.EC2.DetachNetworkInterface
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DetachNetworkInterface where
  type
    Rs DetachNetworkInterface =
      DetachNetworkInterfaceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DetachNetworkInterfaceResponse'

instance Prelude.Hashable DetachNetworkInterface

instance Prelude.NFData DetachNetworkInterface

instance Prelude.ToHeaders DetachNetworkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DetachNetworkInterface where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachNetworkInterface where
  toQuery DetachNetworkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DetachNetworkInterface" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Force" Prelude.=: force,
        "AttachmentId" Prelude.=: attachmentId
      ]

-- | /See:/ 'newDetachNetworkInterfaceResponse' smart constructor.
data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
