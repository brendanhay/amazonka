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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDefaultAuthorizer' smart constructor.
data DescribeDefaultAuthorizer = DescribeDefaultAuthorizer'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeDefaultAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeDefaultAuthorizer ::
  DescribeDefaultAuthorizer
newDescribeDefaultAuthorizer =
  DescribeDefaultAuthorizer'

instance Prelude.AWSRequest DescribeDefaultAuthorizer where
  type
    Rs DescribeDefaultAuthorizer =
      DescribeDefaultAuthorizerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDefaultAuthorizerResponse'
            Prelude.<$> (x Prelude..?> "authorizerDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDefaultAuthorizer

instance Prelude.NFData DescribeDefaultAuthorizer

instance Prelude.ToHeaders DescribeDefaultAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeDefaultAuthorizer where
  toPath = Prelude.const "/default-authorizer"

instance Prelude.ToQuery DescribeDefaultAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDefaultAuthorizerResponse' smart constructor.
data DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse'
  { -- | The default authorizer\'s description.
    authorizerDescription :: Prelude.Maybe AuthorizerDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDefaultAuthorizerResponse
newDescribeDefaultAuthorizerResponse pHttpStatus_ =
  DescribeDefaultAuthorizerResponse'
    { authorizerDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default authorizer\'s description.
describeDefaultAuthorizerResponse_authorizerDescription :: Lens.Lens' DescribeDefaultAuthorizerResponse (Prelude.Maybe AuthorizerDescription)
describeDefaultAuthorizerResponse_authorizerDescription = Lens.lens (\DescribeDefaultAuthorizerResponse' {authorizerDescription} -> authorizerDescription) (\s@DescribeDefaultAuthorizerResponse' {} a -> s {authorizerDescription = a} :: DescribeDefaultAuthorizerResponse)

-- | The response's http status code.
describeDefaultAuthorizerResponse_httpStatus :: Lens.Lens' DescribeDefaultAuthorizerResponse Prelude.Int
describeDefaultAuthorizerResponse_httpStatus = Lens.lens (\DescribeDefaultAuthorizerResponse' {httpStatus} -> httpStatus) (\s@DescribeDefaultAuthorizerResponse' {} a -> s {httpStatus = a} :: DescribeDefaultAuthorizerResponse)

instance
  Prelude.NFData
    DescribeDefaultAuthorizerResponse
