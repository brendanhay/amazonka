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
-- Module      : Amazonka.Connect.DisassociatePhoneNumberContactFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the flow association from a phone number claimed to your Amazon
-- Connect instance.
--
-- If the number is claimed to a traffic distribution group, and you are
-- calling this API using an instance in the Amazon Web Services Region
-- where the traffic distribution group was created, you can use either a
-- full phone number ARN or UUID value for the @PhoneNumberId@ URI request
-- parameter. However, if the number is claimed to a traffic distribution
-- group and you are calling this API using an instance in the alternate
-- Amazon Web Services Region associated with the traffic distribution
-- group, you must provide a full phone number ARN. If a UUID is provided
-- in this scenario, you will receive a @ResourceNotFoundException@.
module Amazonka.Connect.DisassociatePhoneNumberContactFlow
  ( -- * Creating a Request
    DisassociatePhoneNumberContactFlow (..),
    newDisassociatePhoneNumberContactFlow,

    -- * Request Lenses
    disassociatePhoneNumberContactFlow_phoneNumberId,
    disassociatePhoneNumberContactFlow_instanceId,

    -- * Destructuring the Response
    DisassociatePhoneNumberContactFlowResponse (..),
    newDisassociatePhoneNumberContactFlowResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociatePhoneNumberContactFlow' smart constructor.
data DisassociatePhoneNumberContactFlow = DisassociatePhoneNumberContactFlow'
  { -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumberContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'disassociatePhoneNumberContactFlow_phoneNumberId' - A unique identifier for the phone number.
--
-- 'instanceId', 'disassociatePhoneNumberContactFlow_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newDisassociatePhoneNumberContactFlow ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  DisassociatePhoneNumberContactFlow
newDisassociatePhoneNumberContactFlow
  pPhoneNumberId_
  pInstanceId_ =
    DisassociatePhoneNumberContactFlow'
      { phoneNumberId =
          pPhoneNumberId_,
        instanceId = pInstanceId_
      }

-- | A unique identifier for the phone number.
disassociatePhoneNumberContactFlow_phoneNumberId :: Lens.Lens' DisassociatePhoneNumberContactFlow Prelude.Text
disassociatePhoneNumberContactFlow_phoneNumberId = Lens.lens (\DisassociatePhoneNumberContactFlow' {phoneNumberId} -> phoneNumberId) (\s@DisassociatePhoneNumberContactFlow' {} a -> s {phoneNumberId = a} :: DisassociatePhoneNumberContactFlow)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
disassociatePhoneNumberContactFlow_instanceId :: Lens.Lens' DisassociatePhoneNumberContactFlow Prelude.Text
disassociatePhoneNumberContactFlow_instanceId = Lens.lens (\DisassociatePhoneNumberContactFlow' {instanceId} -> instanceId) (\s@DisassociatePhoneNumberContactFlow' {} a -> s {instanceId = a} :: DisassociatePhoneNumberContactFlow)

instance
  Core.AWSRequest
    DisassociatePhoneNumberContactFlow
  where
  type
    AWSResponse DisassociatePhoneNumberContactFlow =
      DisassociatePhoneNumberContactFlowResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociatePhoneNumberContactFlowResponse'

instance
  Prelude.Hashable
    DisassociatePhoneNumberContactFlow
  where
  hashWithSalt
    _salt
    DisassociatePhoneNumberContactFlow' {..} =
      _salt
        `Prelude.hashWithSalt` phoneNumberId
        `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    DisassociatePhoneNumberContactFlow
  where
  rnf DisassociatePhoneNumberContactFlow' {..} =
    Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf instanceId

instance
  Data.ToHeaders
    DisassociatePhoneNumberContactFlow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DisassociatePhoneNumberContactFlow
  where
  toPath DisassociatePhoneNumberContactFlow' {..} =
    Prelude.mconcat
      [ "/phone-number/",
        Data.toBS phoneNumberId,
        "/contact-flow"
      ]

instance
  Data.ToQuery
    DisassociatePhoneNumberContactFlow
  where
  toQuery DisassociatePhoneNumberContactFlow' {..} =
    Prelude.mconcat ["instanceId" Data.=: instanceId]

-- | /See:/ 'newDisassociatePhoneNumberContactFlowResponse' smart constructor.
data DisassociatePhoneNumberContactFlowResponse = DisassociatePhoneNumberContactFlowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePhoneNumberContactFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociatePhoneNumberContactFlowResponse ::
  DisassociatePhoneNumberContactFlowResponse
newDisassociatePhoneNumberContactFlowResponse =
  DisassociatePhoneNumberContactFlowResponse'

instance
  Prelude.NFData
    DisassociatePhoneNumberContactFlowResponse
  where
  rnf _ = ()
