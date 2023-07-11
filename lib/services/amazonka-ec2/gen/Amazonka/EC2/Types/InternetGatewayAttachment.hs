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
-- Module      : Amazonka.EC2.Types.InternetGatewayAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InternetGatewayAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttachmentStatus
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML InternetGatewayAttachment where
  parseXML x =
    InternetGatewayAttachment'
      Prelude.<$> (x Data..@ "state")
      Prelude.<*> (x Data..@ "vpcId")

instance Prelude.Hashable InternetGatewayAttachment where
  hashWithSalt _salt InternetGatewayAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData InternetGatewayAttachment where
  rnf InternetGatewayAttachment' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf vpcId
