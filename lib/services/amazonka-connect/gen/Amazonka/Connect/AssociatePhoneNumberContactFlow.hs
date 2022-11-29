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
-- Module      : Amazonka.Connect.AssociatePhoneNumberContactFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a flow with a phone number claimed to your Amazon Connect
-- instance.
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
module Amazonka.Connect.AssociatePhoneNumberContactFlow
  ( -- * Creating a Request
    AssociatePhoneNumberContactFlow (..),
    newAssociatePhoneNumberContactFlow,

    -- * Request Lenses
    associatePhoneNumberContactFlow_phoneNumberId,
    associatePhoneNumberContactFlow_instanceId,
    associatePhoneNumberContactFlow_contactFlowId,

    -- * Destructuring the Response
    AssociatePhoneNumberContactFlowResponse (..),
    newAssociatePhoneNumberContactFlowResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociatePhoneNumberContactFlow' smart constructor.
data AssociatePhoneNumberContactFlow = AssociatePhoneNumberContactFlow'
  { -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePhoneNumberContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'associatePhoneNumberContactFlow_phoneNumberId' - A unique identifier for the phone number.
--
-- 'instanceId', 'associatePhoneNumberContactFlow_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowId', 'associatePhoneNumberContactFlow_contactFlowId' - The identifier of the flow.
newAssociatePhoneNumberContactFlow ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  AssociatePhoneNumberContactFlow
newAssociatePhoneNumberContactFlow
  pPhoneNumberId_
  pInstanceId_
  pContactFlowId_ =
    AssociatePhoneNumberContactFlow'
      { phoneNumberId =
          pPhoneNumberId_,
        instanceId = pInstanceId_,
        contactFlowId = pContactFlowId_
      }

-- | A unique identifier for the phone number.
associatePhoneNumberContactFlow_phoneNumberId :: Lens.Lens' AssociatePhoneNumberContactFlow Prelude.Text
associatePhoneNumberContactFlow_phoneNumberId = Lens.lens (\AssociatePhoneNumberContactFlow' {phoneNumberId} -> phoneNumberId) (\s@AssociatePhoneNumberContactFlow' {} a -> s {phoneNumberId = a} :: AssociatePhoneNumberContactFlow)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
associatePhoneNumberContactFlow_instanceId :: Lens.Lens' AssociatePhoneNumberContactFlow Prelude.Text
associatePhoneNumberContactFlow_instanceId = Lens.lens (\AssociatePhoneNumberContactFlow' {instanceId} -> instanceId) (\s@AssociatePhoneNumberContactFlow' {} a -> s {instanceId = a} :: AssociatePhoneNumberContactFlow)

-- | The identifier of the flow.
associatePhoneNumberContactFlow_contactFlowId :: Lens.Lens' AssociatePhoneNumberContactFlow Prelude.Text
associatePhoneNumberContactFlow_contactFlowId = Lens.lens (\AssociatePhoneNumberContactFlow' {contactFlowId} -> contactFlowId) (\s@AssociatePhoneNumberContactFlow' {} a -> s {contactFlowId = a} :: AssociatePhoneNumberContactFlow)

instance
  Core.AWSRequest
    AssociatePhoneNumberContactFlow
  where
  type
    AWSResponse AssociatePhoneNumberContactFlow =
      AssociatePhoneNumberContactFlowResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      AssociatePhoneNumberContactFlowResponse'

instance
  Prelude.Hashable
    AssociatePhoneNumberContactFlow
  where
  hashWithSalt
    _salt
    AssociatePhoneNumberContactFlow' {..} =
      _salt `Prelude.hashWithSalt` phoneNumberId
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` contactFlowId

instance
  Prelude.NFData
    AssociatePhoneNumberContactFlow
  where
  rnf AssociatePhoneNumberContactFlow' {..} =
    Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowId

instance
  Core.ToHeaders
    AssociatePhoneNumberContactFlow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociatePhoneNumberContactFlow where
  toJSON AssociatePhoneNumberContactFlow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Core..= instanceId),
            Prelude.Just
              ("ContactFlowId" Core..= contactFlowId)
          ]
      )

instance Core.ToPath AssociatePhoneNumberContactFlow where
  toPath AssociatePhoneNumberContactFlow' {..} =
    Prelude.mconcat
      [ "/phone-number/",
        Core.toBS phoneNumberId,
        "/contact-flow"
      ]

instance Core.ToQuery AssociatePhoneNumberContactFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociatePhoneNumberContactFlowResponse' smart constructor.
data AssociatePhoneNumberContactFlowResponse = AssociatePhoneNumberContactFlowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePhoneNumberContactFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociatePhoneNumberContactFlowResponse ::
  AssociatePhoneNumberContactFlowResponse
newAssociatePhoneNumberContactFlowResponse =
  AssociatePhoneNumberContactFlowResponse'

instance
  Prelude.NFData
    AssociatePhoneNumberContactFlowResponse
  where
  rnf _ = ()
