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
-- Module      : Network.AWS.Connect.DisassociateSecurityKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes the specified security key.
module Network.AWS.Connect.DisassociateSecurityKey
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSecurityKey' smart constructor.
data DisassociateSecurityKey = DisassociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSecurityKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateSecurityKey_instanceId' - The identifier of the Amazon Connect instance.
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

-- | The identifier of the Amazon Connect instance.
disassociateSecurityKey_instanceId :: Lens.Lens' DisassociateSecurityKey Prelude.Text
disassociateSecurityKey_instanceId = Lens.lens (\DisassociateSecurityKey' {instanceId} -> instanceId) (\s@DisassociateSecurityKey' {} a -> s {instanceId = a} :: DisassociateSecurityKey)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
disassociateSecurityKey_associationId :: Lens.Lens' DisassociateSecurityKey Prelude.Text
disassociateSecurityKey_associationId = Lens.lens (\DisassociateSecurityKey' {associationId} -> associationId) (\s@DisassociateSecurityKey' {} a -> s {associationId = a} :: DisassociateSecurityKey)

instance Prelude.AWSRequest DisassociateSecurityKey where
  type
    Rs DisassociateSecurityKey =
      DisassociateSecurityKeyResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DisassociateSecurityKeyResponse'

instance Prelude.Hashable DisassociateSecurityKey

instance Prelude.NFData DisassociateSecurityKey

instance Prelude.ToHeaders DisassociateSecurityKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DisassociateSecurityKey where
  toPath DisassociateSecurityKey' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/security-key/",
        Prelude.toBS associationId
      ]

instance Prelude.ToQuery DisassociateSecurityKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSecurityKeyResponse' smart constructor.
data DisassociateSecurityKeyResponse = DisassociateSecurityKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
