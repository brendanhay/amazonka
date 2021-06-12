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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes related to AWS Elastic Beanstalk that are associated
-- with the calling AWS account.
--
-- The result currently has one set of attributes—resource quotas.
module Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_resourceQuotas,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAccountAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountAttributes ::
  DescribeAccountAttributes
newDescribeAccountAttributes =
  DescribeAccountAttributes'

instance Core.AWSRequest DescribeAccountAttributes where
  type
    AWSResponse DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Core.<$> (x Core..@? "ResourceQuotas")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAccountAttributes

instance Core.NFData DescribeAccountAttributes

instance Core.ToHeaders DescribeAccountAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAccountAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAccountAttributes where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeAccountAttributes" :: Core.ByteString),
            "Version" Core.=: ("2010-12-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | The Elastic Beanstalk resource quotas associated with the calling AWS
    -- account.
    resourceQuotas :: Core.Maybe ResourceQuotas,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAccountAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceQuotas', 'describeAccountAttributesResponse_resourceQuotas' - The Elastic Beanstalk resource quotas associated with the calling AWS
-- account.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { resourceQuotas =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Elastic Beanstalk resource quotas associated with the calling AWS
-- account.
describeAccountAttributesResponse_resourceQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe ResourceQuotas)
describeAccountAttributesResponse_resourceQuotas = Lens.lens (\DescribeAccountAttributesResponse' {resourceQuotas} -> resourceQuotas) (\s@DescribeAccountAttributesResponse' {} a -> s {resourceQuotas = a} :: DescribeAccountAttributesResponse)

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Core.NFData
    DescribeAccountAttributesResponse
