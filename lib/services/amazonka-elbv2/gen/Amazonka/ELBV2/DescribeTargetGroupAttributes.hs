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
-- Module      : Amazonka.ELBV2.DescribeTargetGroupAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified target group.
--
-- For more information, see the following:
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html#target-group-attributes Target group attributes>
--     in the /Application Load Balancers Guide/
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html#target-group-attributes Target group attributes>
--     in the /Network Load Balancers Guide/
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/target-groups.html#target-group-attributes Target group attributes>
--     in the /Gateway Load Balancers Guide/
module Amazonka.ELBV2.DescribeTargetGroupAttributes
  ( -- * Creating a Request
    DescribeTargetGroupAttributes (..),
    newDescribeTargetGroupAttributes,

    -- * Request Lenses
    describeTargetGroupAttributes_targetGroupArn,

    -- * Destructuring the Response
    DescribeTargetGroupAttributesResponse (..),
    newDescribeTargetGroupAttributesResponse,

    -- * Response Lenses
    describeTargetGroupAttributesResponse_attributes,
    describeTargetGroupAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTargetGroupAttributes' smart constructor.
data DescribeTargetGroupAttributes = DescribeTargetGroupAttributes'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTargetGroupAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupArn', 'describeTargetGroupAttributes_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
newDescribeTargetGroupAttributes ::
  -- | 'targetGroupArn'
  Prelude.Text ->
  DescribeTargetGroupAttributes
newDescribeTargetGroupAttributes pTargetGroupArn_ =
  DescribeTargetGroupAttributes'
    { targetGroupArn =
        pTargetGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the target group.
describeTargetGroupAttributes_targetGroupArn :: Lens.Lens' DescribeTargetGroupAttributes Prelude.Text
describeTargetGroupAttributes_targetGroupArn = Lens.lens (\DescribeTargetGroupAttributes' {targetGroupArn} -> targetGroupArn) (\s@DescribeTargetGroupAttributes' {} a -> s {targetGroupArn = a} :: DescribeTargetGroupAttributes)

instance
  Core.AWSRequest
    DescribeTargetGroupAttributes
  where
  type
    AWSResponse DescribeTargetGroupAttributes =
      DescribeTargetGroupAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTargetGroupAttributesResult"
      ( \s h x ->
          DescribeTargetGroupAttributesResponse'
            Prelude.<$> ( x Core..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTargetGroupAttributes
  where
  hashWithSalt _salt DescribeTargetGroupAttributes' {..} =
    _salt `Prelude.hashWithSalt` targetGroupArn

instance Prelude.NFData DescribeTargetGroupAttributes where
  rnf DescribeTargetGroupAttributes' {..} =
    Prelude.rnf targetGroupArn

instance Core.ToHeaders DescribeTargetGroupAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTargetGroupAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTargetGroupAttributes where
  toQuery DescribeTargetGroupAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTargetGroupAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Core.=: targetGroupArn
      ]

-- | /See:/ 'newDescribeTargetGroupAttributesResponse' smart constructor.
data DescribeTargetGroupAttributesResponse = DescribeTargetGroupAttributesResponse'
  { -- | Information about the target group attributes
    attributes :: Prelude.Maybe [TargetGroupAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTargetGroupAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'describeTargetGroupAttributesResponse_attributes' - Information about the target group attributes
--
-- 'httpStatus', 'describeTargetGroupAttributesResponse_httpStatus' - The response's http status code.
newDescribeTargetGroupAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTargetGroupAttributesResponse
newDescribeTargetGroupAttributesResponse pHttpStatus_ =
  DescribeTargetGroupAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the target group attributes
describeTargetGroupAttributesResponse_attributes :: Lens.Lens' DescribeTargetGroupAttributesResponse (Prelude.Maybe [TargetGroupAttribute])
describeTargetGroupAttributesResponse_attributes = Lens.lens (\DescribeTargetGroupAttributesResponse' {attributes} -> attributes) (\s@DescribeTargetGroupAttributesResponse' {} a -> s {attributes = a} :: DescribeTargetGroupAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTargetGroupAttributesResponse_httpStatus :: Lens.Lens' DescribeTargetGroupAttributesResponse Prelude.Int
describeTargetGroupAttributesResponse_httpStatus = Lens.lens (\DescribeTargetGroupAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeTargetGroupAttributesResponse' {} a -> s {httpStatus = a} :: DescribeTargetGroupAttributesResponse)

instance
  Prelude.NFData
    DescribeTargetGroupAttributesResponse
  where
  rnf DescribeTargetGroupAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
