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
-- Module      : Amazonka.LakeFormation.DescribeResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current data access role for the given resource registered
-- in Lake Formation.
module Amazonka.LakeFormation.DescribeResource
  ( -- * Creating a Request
    DescribeResource (..),
    newDescribeResource,

    -- * Request Lenses
    describeResource_resourceArn,

    -- * Destructuring the Response
    DescribeResourceResponse (..),
    newDescribeResourceResponse,

    -- * Response Lenses
    describeResourceResponse_resourceInfo,
    describeResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeResource' smart constructor.
data DescribeResource = DescribeResource'
  { -- | The resource ARN.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'describeResource_resourceArn' - The resource ARN.
newDescribeResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  DescribeResource
newDescribeResource pResourceArn_ =
  DescribeResource' {resourceArn = pResourceArn_}

-- | The resource ARN.
describeResource_resourceArn :: Lens.Lens' DescribeResource Prelude.Text
describeResource_resourceArn = Lens.lens (\DescribeResource' {resourceArn} -> resourceArn) (\s@DescribeResource' {} a -> s {resourceArn = a} :: DescribeResource)

instance Core.AWSRequest DescribeResource where
  type
    AWSResponse DescribeResource =
      DescribeResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceResponse'
            Prelude.<$> (x Data..?> "ResourceInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResource where
  hashWithSalt _salt DescribeResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DescribeResource where
  rnf DescribeResource' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders DescribeResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeResource where
  toJSON DescribeResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DescribeResource where
  toPath = Prelude.const "/DescribeResource"

instance Data.ToQuery DescribeResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { -- | A structure containing information about an Lake Formation resource.
    resourceInfo :: Prelude.Maybe ResourceInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceInfo', 'describeResourceResponse_resourceInfo' - A structure containing information about an Lake Formation resource.
--
-- 'httpStatus', 'describeResourceResponse_httpStatus' - The response's http status code.
newDescribeResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourceResponse
newDescribeResourceResponse pHttpStatus_ =
  DescribeResourceResponse'
    { resourceInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing information about an Lake Formation resource.
describeResourceResponse_resourceInfo :: Lens.Lens' DescribeResourceResponse (Prelude.Maybe ResourceInfo)
describeResourceResponse_resourceInfo = Lens.lens (\DescribeResourceResponse' {resourceInfo} -> resourceInfo) (\s@DescribeResourceResponse' {} a -> s {resourceInfo = a} :: DescribeResourceResponse)

-- | The response's http status code.
describeResourceResponse_httpStatus :: Lens.Lens' DescribeResourceResponse Prelude.Int
describeResourceResponse_httpStatus = Lens.lens (\DescribeResourceResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceResponse' {} a -> s {httpStatus = a} :: DescribeResourceResponse)

instance Prelude.NFData DescribeResourceResponse where
  rnf DescribeResourceResponse' {..} =
    Prelude.rnf resourceInfo
      `Prelude.seq` Prelude.rnf httpStatus
