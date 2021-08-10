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
-- Module      : Network.AWS.Connect.DescribeContactFlow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified contact flow.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Network.AWS.Connect.DescribeContactFlow
  ( -- * Creating a Request
    DescribeContactFlow (..),
    newDescribeContactFlow,

    -- * Request Lenses
    describeContactFlow_instanceId,
    describeContactFlow_contactFlowId,

    -- * Destructuring the Response
    DescribeContactFlowResponse (..),
    newDescribeContactFlowResponse,

    -- * Response Lenses
    describeContactFlowResponse_contactFlow,
    describeContactFlowResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeContactFlow' smart constructor.
data DescribeContactFlow = DescribeContactFlow'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeContactFlow_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactFlowId', 'describeContactFlow_contactFlowId' - The identifier of the contact flow.
newDescribeContactFlow ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  DescribeContactFlow
newDescribeContactFlow pInstanceId_ pContactFlowId_ =
  DescribeContactFlow'
    { instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_
    }

-- | The identifier of the Amazon Connect instance.
describeContactFlow_instanceId :: Lens.Lens' DescribeContactFlow Prelude.Text
describeContactFlow_instanceId = Lens.lens (\DescribeContactFlow' {instanceId} -> instanceId) (\s@DescribeContactFlow' {} a -> s {instanceId = a} :: DescribeContactFlow)

-- | The identifier of the contact flow.
describeContactFlow_contactFlowId :: Lens.Lens' DescribeContactFlow Prelude.Text
describeContactFlow_contactFlowId = Lens.lens (\DescribeContactFlow' {contactFlowId} -> contactFlowId) (\s@DescribeContactFlow' {} a -> s {contactFlowId = a} :: DescribeContactFlow)

instance Core.AWSRequest DescribeContactFlow where
  type
    AWSResponse DescribeContactFlow =
      DescribeContactFlowResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContactFlowResponse'
            Prelude.<$> (x Core..?> "ContactFlow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContactFlow

instance Prelude.NFData DescribeContactFlow

instance Core.ToHeaders DescribeContactFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeContactFlow where
  toPath DescribeContactFlow' {..} =
    Prelude.mconcat
      [ "/contact-flows/",
        Core.toBS instanceId,
        "/",
        Core.toBS contactFlowId
      ]

instance Core.ToQuery DescribeContactFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContactFlowResponse' smart constructor.
data DescribeContactFlowResponse = DescribeContactFlowResponse'
  { -- | Information about the contact flow.
    contactFlow :: Prelude.Maybe ContactFlow,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFlow', 'describeContactFlowResponse_contactFlow' - Information about the contact flow.
--
-- 'httpStatus', 'describeContactFlowResponse_httpStatus' - The response's http status code.
newDescribeContactFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContactFlowResponse
newDescribeContactFlowResponse pHttpStatus_ =
  DescribeContactFlowResponse'
    { contactFlow =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the contact flow.
describeContactFlowResponse_contactFlow :: Lens.Lens' DescribeContactFlowResponse (Prelude.Maybe ContactFlow)
describeContactFlowResponse_contactFlow = Lens.lens (\DescribeContactFlowResponse' {contactFlow} -> contactFlow) (\s@DescribeContactFlowResponse' {} a -> s {contactFlow = a} :: DescribeContactFlowResponse)

-- | The response's http status code.
describeContactFlowResponse_httpStatus :: Lens.Lens' DescribeContactFlowResponse Prelude.Int
describeContactFlowResponse_httpStatus = Lens.lens (\DescribeContactFlowResponse' {httpStatus} -> httpStatus) (\s@DescribeContactFlowResponse' {} a -> s {httpStatus = a} :: DescribeContactFlowResponse)

instance Prelude.NFData DescribeContactFlowResponse
