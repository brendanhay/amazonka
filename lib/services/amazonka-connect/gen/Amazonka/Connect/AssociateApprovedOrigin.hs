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
-- Module      : Amazonka.Connect.AssociateApprovedOrigin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Associates an approved origin to an Amazon Connect instance.
module Amazonka.Connect.AssociateApprovedOrigin
  ( -- * Creating a Request
    AssociateApprovedOrigin (..),
    newAssociateApprovedOrigin,

    -- * Request Lenses
    associateApprovedOrigin_instanceId,
    associateApprovedOrigin_origin,

    -- * Destructuring the Response
    AssociateApprovedOriginResponse (..),
    newAssociateApprovedOriginResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateApprovedOrigin' smart constructor.
data AssociateApprovedOrigin = AssociateApprovedOrigin'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The domain to add to your allow list.
    origin :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApprovedOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateApprovedOrigin_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'origin', 'associateApprovedOrigin_origin' - The domain to add to your allow list.
newAssociateApprovedOrigin ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'origin'
  Prelude.Text ->
  AssociateApprovedOrigin
newAssociateApprovedOrigin pInstanceId_ pOrigin_ =
  AssociateApprovedOrigin'
    { instanceId = pInstanceId_,
      origin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
associateApprovedOrigin_instanceId :: Lens.Lens' AssociateApprovedOrigin Prelude.Text
associateApprovedOrigin_instanceId = Lens.lens (\AssociateApprovedOrigin' {instanceId} -> instanceId) (\s@AssociateApprovedOrigin' {} a -> s {instanceId = a} :: AssociateApprovedOrigin)

-- | The domain to add to your allow list.
associateApprovedOrigin_origin :: Lens.Lens' AssociateApprovedOrigin Prelude.Text
associateApprovedOrigin_origin = Lens.lens (\AssociateApprovedOrigin' {origin} -> origin) (\s@AssociateApprovedOrigin' {} a -> s {origin = a} :: AssociateApprovedOrigin)

instance Core.AWSRequest AssociateApprovedOrigin where
  type
    AWSResponse AssociateApprovedOrigin =
      AssociateApprovedOriginResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      AssociateApprovedOriginResponse'

instance Prelude.Hashable AssociateApprovedOrigin where
  hashWithSalt _salt AssociateApprovedOrigin' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` origin

instance Prelude.NFData AssociateApprovedOrigin where
  rnf AssociateApprovedOrigin' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf origin

instance Data.ToHeaders AssociateApprovedOrigin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateApprovedOrigin where
  toJSON AssociateApprovedOrigin' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Origin" Data..= origin)]
      )

instance Data.ToPath AssociateApprovedOrigin where
  toPath AssociateApprovedOrigin' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/approved-origin"
      ]

instance Data.ToQuery AssociateApprovedOrigin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApprovedOriginResponse' smart constructor.
data AssociateApprovedOriginResponse = AssociateApprovedOriginResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApprovedOriginResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateApprovedOriginResponse ::
  AssociateApprovedOriginResponse
newAssociateApprovedOriginResponse =
  AssociateApprovedOriginResponse'

instance
  Prelude.NFData
    AssociateApprovedOriginResponse
  where
  rnf _ = ()
