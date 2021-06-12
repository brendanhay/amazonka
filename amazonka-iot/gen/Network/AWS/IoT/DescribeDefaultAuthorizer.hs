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
-- Module      : Network.AWS.IoT.DescribeDefaultAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default authorizer.
module Network.AWS.IoT.DescribeDefaultAuthorizer
  ( -- * Creating a Request
    DescribeDefaultAuthorizer (..),
    newDescribeDefaultAuthorizer,

    -- * Destructuring the Response
    DescribeDefaultAuthorizerResponse (..),
    newDescribeDefaultAuthorizerResponse,

    -- * Response Lenses
    describeDefaultAuthorizerResponse_authorizerDescription,
    describeDefaultAuthorizerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDefaultAuthorizer' smart constructor.
data DescribeDefaultAuthorizer = DescribeDefaultAuthorizer'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDefaultAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeDefaultAuthorizer ::
  DescribeDefaultAuthorizer
newDescribeDefaultAuthorizer =
  DescribeDefaultAuthorizer'

instance Core.AWSRequest DescribeDefaultAuthorizer where
  type
    AWSResponse DescribeDefaultAuthorizer =
      DescribeDefaultAuthorizerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDefaultAuthorizerResponse'
            Core.<$> (x Core..?> "authorizerDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDefaultAuthorizer

instance Core.NFData DescribeDefaultAuthorizer

instance Core.ToHeaders DescribeDefaultAuthorizer where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDefaultAuthorizer where
  toPath = Core.const "/default-authorizer"

instance Core.ToQuery DescribeDefaultAuthorizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDefaultAuthorizerResponse' smart constructor.
data DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse'
  { -- | The default authorizer\'s description.
    authorizerDescription :: Core.Maybe AuthorizerDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDefaultAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerDescription', 'describeDefaultAuthorizerResponse_authorizerDescription' - The default authorizer\'s description.
--
-- 'httpStatus', 'describeDefaultAuthorizerResponse_httpStatus' - The response's http status code.
newDescribeDefaultAuthorizerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDefaultAuthorizerResponse
newDescribeDefaultAuthorizerResponse pHttpStatus_ =
  DescribeDefaultAuthorizerResponse'
    { authorizerDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default authorizer\'s description.
describeDefaultAuthorizerResponse_authorizerDescription :: Lens.Lens' DescribeDefaultAuthorizerResponse (Core.Maybe AuthorizerDescription)
describeDefaultAuthorizerResponse_authorizerDescription = Lens.lens (\DescribeDefaultAuthorizerResponse' {authorizerDescription} -> authorizerDescription) (\s@DescribeDefaultAuthorizerResponse' {} a -> s {authorizerDescription = a} :: DescribeDefaultAuthorizerResponse)

-- | The response's http status code.
describeDefaultAuthorizerResponse_httpStatus :: Lens.Lens' DescribeDefaultAuthorizerResponse Core.Int
describeDefaultAuthorizerResponse_httpStatus = Lens.lens (\DescribeDefaultAuthorizerResponse' {httpStatus} -> httpStatus) (\s@DescribeDefaultAuthorizerResponse' {} a -> s {httpStatus = a} :: DescribeDefaultAuthorizerResponse)

instance
  Core.NFData
    DescribeDefaultAuthorizerResponse
