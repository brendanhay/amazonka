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
-- Module      : Network.AWS.IoT.DescribeAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an authorizer.
module Network.AWS.IoT.DescribeAuthorizer
  ( -- * Creating a Request
    DescribeAuthorizer (..),
    newDescribeAuthorizer,

    -- * Request Lenses
    describeAuthorizer_authorizerName,

    -- * Destructuring the Response
    DescribeAuthorizerResponse (..),
    newDescribeAuthorizerResponse,

    -- * Response Lenses
    describeAuthorizerResponse_authorizerDescription,
    describeAuthorizerResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAuthorizer' smart constructor.
data DescribeAuthorizer = DescribeAuthorizer'
  { -- | The name of the authorizer to describe.
    authorizerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerName', 'describeAuthorizer_authorizerName' - The name of the authorizer to describe.
newDescribeAuthorizer ::
  -- | 'authorizerName'
  Prelude.Text ->
  DescribeAuthorizer
newDescribeAuthorizer pAuthorizerName_ =
  DescribeAuthorizer'
    { authorizerName =
        pAuthorizerName_
    }

-- | The name of the authorizer to describe.
describeAuthorizer_authorizerName :: Lens.Lens' DescribeAuthorizer Prelude.Text
describeAuthorizer_authorizerName = Lens.lens (\DescribeAuthorizer' {authorizerName} -> authorizerName) (\s@DescribeAuthorizer' {} a -> s {authorizerName = a} :: DescribeAuthorizer)

instance Prelude.AWSRequest DescribeAuthorizer where
  type
    Rs DescribeAuthorizer =
      DescribeAuthorizerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuthorizerResponse'
            Prelude.<$> (x Prelude..?> "authorizerDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAuthorizer

instance Prelude.NFData DescribeAuthorizer

instance Prelude.ToHeaders DescribeAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeAuthorizer where
  toPath DescribeAuthorizer' {..} =
    Prelude.mconcat
      ["/authorizer/", Prelude.toBS authorizerName]

instance Prelude.ToQuery DescribeAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAuthorizerResponse' smart constructor.
data DescribeAuthorizerResponse = DescribeAuthorizerResponse'
  { -- | The authorizer description.
    authorizerDescription :: Prelude.Maybe AuthorizerDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerDescription', 'describeAuthorizerResponse_authorizerDescription' - The authorizer description.
--
-- 'httpStatus', 'describeAuthorizerResponse_httpStatus' - The response's http status code.
newDescribeAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuthorizerResponse
newDescribeAuthorizerResponse pHttpStatus_ =
  DescribeAuthorizerResponse'
    { authorizerDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorizer description.
describeAuthorizerResponse_authorizerDescription :: Lens.Lens' DescribeAuthorizerResponse (Prelude.Maybe AuthorizerDescription)
describeAuthorizerResponse_authorizerDescription = Lens.lens (\DescribeAuthorizerResponse' {authorizerDescription} -> authorizerDescription) (\s@DescribeAuthorizerResponse' {} a -> s {authorizerDescription = a} :: DescribeAuthorizerResponse)

-- | The response's http status code.
describeAuthorizerResponse_httpStatus :: Lens.Lens' DescribeAuthorizerResponse Prelude.Int
describeAuthorizerResponse_httpStatus = Lens.lens (\DescribeAuthorizerResponse' {httpStatus} -> httpStatus) (\s@DescribeAuthorizerResponse' {} a -> s {httpStatus = a} :: DescribeAuthorizerResponse)

instance Prelude.NFData DescribeAuthorizerResponse
