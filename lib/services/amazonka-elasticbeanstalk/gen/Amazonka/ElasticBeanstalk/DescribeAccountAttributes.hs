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
-- Module      : Amazonka.ElasticBeanstalk.DescribeAccountAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes related to AWS Elastic Beanstalk that are associated
-- with the calling AWS account.
--
-- The result currently has one set of attributes—resource quotas.
module Amazonka.ElasticBeanstalk.DescribeAccountAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Prelude.<$> (x Data..@? "ResourceQuotas")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountAttributes where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeAccountAttributes where
  rnf _ = ()

instance Data.ToHeaders DescribeAccountAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccountAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountAttributes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("DescribeAccountAttributes" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | The Elastic Beanstalk resource quotas associated with the calling AWS
    -- account.
    resourceQuotas :: Prelude.Maybe ResourceQuotas,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { resourceQuotas =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Elastic Beanstalk resource quotas associated with the calling AWS
-- account.
describeAccountAttributesResponse_resourceQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Prelude.Maybe ResourceQuotas)
describeAccountAttributesResponse_resourceQuotas = Lens.lens (\DescribeAccountAttributesResponse' {resourceQuotas} -> resourceQuotas) (\s@DescribeAccountAttributesResponse' {} a -> s {resourceQuotas = a} :: DescribeAccountAttributesResponse)

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Prelude.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Prelude.NFData
    DescribeAccountAttributesResponse
  where
  rnf DescribeAccountAttributesResponse' {..} =
    Prelude.rnf resourceQuotas
      `Prelude.seq` Prelude.rnf httpStatus
