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
-- Module      : Amazonka.EC2.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets permission settings for the specified snapshot.
--
-- For more information about modifying snapshot permissions, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Share a snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.ResetSnapshotAttribute
  ( -- * Creating a Request
    ResetSnapshotAttribute (..),
    newResetSnapshotAttribute,

    -- * Request Lenses
    resetSnapshotAttribute_dryRun,
    resetSnapshotAttribute_attribute,
    resetSnapshotAttribute_snapshotId,

    -- * Destructuring the Response
    ResetSnapshotAttributeResponse (..),
    newResetSnapshotAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetSnapshotAttribute' smart constructor.
data ResetSnapshotAttribute = ResetSnapshotAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The attribute to reset. Currently, only the attribute for permission to
    -- create volumes can be reset.
    attribute :: SnapshotAttributeName,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetSnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'resetSnapshotAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'resetSnapshotAttribute_attribute' - The attribute to reset. Currently, only the attribute for permission to
-- create volumes can be reset.
--
-- 'snapshotId', 'resetSnapshotAttribute_snapshotId' - The ID of the snapshot.
newResetSnapshotAttribute ::
  -- | 'attribute'
  SnapshotAttributeName ->
  -- | 'snapshotId'
  Prelude.Text ->
  ResetSnapshotAttribute
newResetSnapshotAttribute pAttribute_ pSnapshotId_ =
  ResetSnapshotAttribute'
    { dryRun = Prelude.Nothing,
      attribute = pAttribute_,
      snapshotId = pSnapshotId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetSnapshotAttribute_dryRun :: Lens.Lens' ResetSnapshotAttribute (Prelude.Maybe Prelude.Bool)
resetSnapshotAttribute_dryRun = Lens.lens (\ResetSnapshotAttribute' {dryRun} -> dryRun) (\s@ResetSnapshotAttribute' {} a -> s {dryRun = a} :: ResetSnapshotAttribute)

-- | The attribute to reset. Currently, only the attribute for permission to
-- create volumes can be reset.
resetSnapshotAttribute_attribute :: Lens.Lens' ResetSnapshotAttribute SnapshotAttributeName
resetSnapshotAttribute_attribute = Lens.lens (\ResetSnapshotAttribute' {attribute} -> attribute) (\s@ResetSnapshotAttribute' {} a -> s {attribute = a} :: ResetSnapshotAttribute)

-- | The ID of the snapshot.
resetSnapshotAttribute_snapshotId :: Lens.Lens' ResetSnapshotAttribute Prelude.Text
resetSnapshotAttribute_snapshotId = Lens.lens (\ResetSnapshotAttribute' {snapshotId} -> snapshotId) (\s@ResetSnapshotAttribute' {} a -> s {snapshotId = a} :: ResetSnapshotAttribute)

instance Core.AWSRequest ResetSnapshotAttribute where
  type
    AWSResponse ResetSnapshotAttribute =
      ResetSnapshotAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ResetSnapshotAttributeResponse'

instance Prelude.Hashable ResetSnapshotAttribute where
  hashWithSalt _salt ResetSnapshotAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData ResetSnapshotAttribute where
  rnf ResetSnapshotAttribute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders ResetSnapshotAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetSnapshotAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetSnapshotAttribute where
  toQuery ResetSnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetSnapshotAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Attribute" Data.=: attribute,
        "SnapshotId" Data.=: snapshotId
      ]

-- | /See:/ 'newResetSnapshotAttributeResponse' smart constructor.
data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetSnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetSnapshotAttributeResponse ::
  ResetSnapshotAttributeResponse
newResetSnapshotAttributeResponse =
  ResetSnapshotAttributeResponse'

instance
  Prelude.NFData
    ResetSnapshotAttributeResponse
  where
  rnf _ = ()
