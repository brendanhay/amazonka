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
-- Module      : Amazonka.Connect.DisassociateApprovedOrigin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Revokes access to integrated applications from Amazon Connect.
module Amazonka.Connect.DisassociateApprovedOrigin
  ( -- * Creating a Request
    DisassociateApprovedOrigin (..),
    newDisassociateApprovedOrigin,

    -- * Request Lenses
    disassociateApprovedOrigin_instanceId,
    disassociateApprovedOrigin_origin,

    -- * Destructuring the Response
    DisassociateApprovedOriginResponse (..),
    newDisassociateApprovedOriginResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateApprovedOrigin' smart constructor.
data DisassociateApprovedOrigin = DisassociateApprovedOrigin'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The domain URL of the integrated application.
    origin :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApprovedOrigin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateApprovedOrigin_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'origin', 'disassociateApprovedOrigin_origin' - The domain URL of the integrated application.
newDisassociateApprovedOrigin ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'origin'
  Prelude.Text ->
  DisassociateApprovedOrigin
newDisassociateApprovedOrigin pInstanceId_ pOrigin_ =
  DisassociateApprovedOrigin'
    { instanceId =
        pInstanceId_,
      origin = pOrigin_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
disassociateApprovedOrigin_instanceId :: Lens.Lens' DisassociateApprovedOrigin Prelude.Text
disassociateApprovedOrigin_instanceId = Lens.lens (\DisassociateApprovedOrigin' {instanceId} -> instanceId) (\s@DisassociateApprovedOrigin' {} a -> s {instanceId = a} :: DisassociateApprovedOrigin)

-- | The domain URL of the integrated application.
disassociateApprovedOrigin_origin :: Lens.Lens' DisassociateApprovedOrigin Prelude.Text
disassociateApprovedOrigin_origin = Lens.lens (\DisassociateApprovedOrigin' {origin} -> origin) (\s@DisassociateApprovedOrigin' {} a -> s {origin = a} :: DisassociateApprovedOrigin)

instance Core.AWSRequest DisassociateApprovedOrigin where
  type
    AWSResponse DisassociateApprovedOrigin =
      DisassociateApprovedOriginResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateApprovedOriginResponse'

instance Prelude.Hashable DisassociateApprovedOrigin where
  hashWithSalt _salt DisassociateApprovedOrigin' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` origin

instance Prelude.NFData DisassociateApprovedOrigin where
  rnf DisassociateApprovedOrigin' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf origin

instance Data.ToHeaders DisassociateApprovedOrigin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateApprovedOrigin where
  toPath DisassociateApprovedOrigin' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/approved-origin"
      ]

instance Data.ToQuery DisassociateApprovedOrigin where
  toQuery DisassociateApprovedOrigin' {..} =
    Prelude.mconcat ["origin" Data.=: origin]

-- | /See:/ 'newDisassociateApprovedOriginResponse' smart constructor.
data DisassociateApprovedOriginResponse = DisassociateApprovedOriginResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApprovedOriginResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateApprovedOriginResponse ::
  DisassociateApprovedOriginResponse
newDisassociateApprovedOriginResponse =
  DisassociateApprovedOriginResponse'

instance
  Prelude.NFData
    DisassociateApprovedOriginResponse
  where
  rnf _ = ()
