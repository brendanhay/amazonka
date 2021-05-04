{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.InternetGatewayAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InternetGatewayAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the attachment of a VPC to an internet gateway or an
-- egress-only internet gateway.
--
-- /See:/ 'newInternetGatewayAttachment' smart constructor.
data InternetGatewayAttachment = InternetGatewayAttachment'
  { -- | The current state of the attachment. For an internet gateway, the state
    -- is @available@ when attached to a VPC; otherwise, this value is not
    -- returned.
    state :: AttachmentStatus,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InternetGatewayAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'internetGatewayAttachment_state' - The current state of the attachment. For an internet gateway, the state
-- is @available@ when attached to a VPC; otherwise, this value is not
-- returned.
--
-- 'vpcId', 'internetGatewayAttachment_vpcId' - The ID of the VPC.
newInternetGatewayAttachment ::
  -- | 'state'
  AttachmentStatus ->
  -- | 'vpcId'
  Prelude.Text ->
  InternetGatewayAttachment
newInternetGatewayAttachment pState_ pVpcId_ =
  InternetGatewayAttachment'
    { state = pState_,
      vpcId = pVpcId_
    }

-- | The current state of the attachment. For an internet gateway, the state
-- is @available@ when attached to a VPC; otherwise, this value is not
-- returned.
internetGatewayAttachment_state :: Lens.Lens' InternetGatewayAttachment AttachmentStatus
internetGatewayAttachment_state = Lens.lens (\InternetGatewayAttachment' {state} -> state) (\s@InternetGatewayAttachment' {} a -> s {state = a} :: InternetGatewayAttachment)

-- | The ID of the VPC.
internetGatewayAttachment_vpcId :: Lens.Lens' InternetGatewayAttachment Prelude.Text
internetGatewayAttachment_vpcId = Lens.lens (\InternetGatewayAttachment' {vpcId} -> vpcId) (\s@InternetGatewayAttachment' {} a -> s {vpcId = a} :: InternetGatewayAttachment)

instance Prelude.FromXML InternetGatewayAttachment where
  parseXML x =
    InternetGatewayAttachment'
      Prelude.<$> (x Prelude..@ "state")
      Prelude.<*> (x Prelude..@ "vpcId")

instance Prelude.Hashable InternetGatewayAttachment

instance Prelude.NFData InternetGatewayAttachment
