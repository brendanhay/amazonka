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
-- Module      : Amazonka.Connect.DisassociateSecurityKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes the specified security key.
module Amazonka.Connect.DisassociateSecurityKey
  ( -- * Creating a Request
    DisassociateSecurityKey (..),
    newDisassociateSecurityKey,

    -- * Request Lenses
    disassociateSecurityKey_instanceId,
    disassociateSecurityKey_associationId,

    -- * Destructuring the Response
    DisassociateSecurityKeyResponse (..),
    newDisassociateSecurityKeyResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateSecurityKey' smart constructor.
data DisassociateSecurityKey = DisassociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSecurityKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateSecurityKey_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'associationId', 'disassociateSecurityKey_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
newDisassociateSecurityKey ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'associationId'
  Prelude.Text ->
  DisassociateSecurityKey
newDisassociateSecurityKey
  pInstanceId_
  pAssociationId_ =
    DisassociateSecurityKey'
      { instanceId = pInstanceId_,
        associationId = pAssociationId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
disassociateSecurityKey_instanceId :: Lens.Lens' DisassociateSecurityKey Prelude.Text
disassociateSecurityKey_instanceId = Lens.lens (\DisassociateSecurityKey' {instanceId} -> instanceId) (\s@DisassociateSecurityKey' {} a -> s {instanceId = a} :: DisassociateSecurityKey)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
disassociateSecurityKey_associationId :: Lens.Lens' DisassociateSecurityKey Prelude.Text
disassociateSecurityKey_associationId = Lens.lens (\DisassociateSecurityKey' {associationId} -> associationId) (\s@DisassociateSecurityKey' {} a -> s {associationId = a} :: DisassociateSecurityKey)

instance Core.AWSRequest DisassociateSecurityKey where
  type
    AWSResponse DisassociateSecurityKey =
      DisassociateSecurityKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateSecurityKeyResponse'

instance Prelude.Hashable DisassociateSecurityKey where
  hashWithSalt _salt DisassociateSecurityKey' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData DisassociateSecurityKey where
  rnf DisassociateSecurityKey' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf associationId

instance Data.ToHeaders DisassociateSecurityKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateSecurityKey where
  toPath DisassociateSecurityKey' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/security-key/",
        Data.toBS associationId
      ]

instance Data.ToQuery DisassociateSecurityKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSecurityKeyResponse' smart constructor.
data DisassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSecurityKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateSecurityKeyResponse ::
  DisassociateSecurityKeyResponse
newDisassociateSecurityKeyResponse =
  DisassociateSecurityKeyResponse'

instance
  Prelude.NFData
    DisassociateSecurityKeyResponse
  where
  rnf _ = ()
