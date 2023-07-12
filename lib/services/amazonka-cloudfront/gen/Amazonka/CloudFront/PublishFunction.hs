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
-- Module      : Amazonka.CloudFront.PublishFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a CloudFront function by copying the function code from the
-- @DEVELOPMENT@ stage to @LIVE@. This automatically updates all cache
-- behaviors that are using this function to use the newly published copy
-- in the @LIVE@ stage.
--
-- When a function is published to the @LIVE@ stage, you can attach the
-- function to a distribution\'s cache behavior, using the function\'s
-- Amazon Resource Name (ARN).
--
-- To publish a function, you must provide the function\'s name and version
-- (@ETag@ value). To get these values, you can use @ListFunctions@ and
-- @DescribeFunction@.
module Amazonka.CloudFront.PublishFunction
  ( -- * Creating a Request
    PublishFunction (..),
    newPublishFunction,

    -- * Request Lenses
    publishFunction_name,
    publishFunction_ifMatch,

    -- * Destructuring the Response
    PublishFunctionResponse (..),
    newPublishFunctionResponse,

    -- * Response Lenses
    publishFunctionResponse_functionSummary,
    publishFunctionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishFunction' smart constructor.
data PublishFunction = PublishFunction'
  { -- | The name of the function that you are publishing.
    name :: Prelude.Text,
    -- | The current version (@ETag@ value) of the function that you are
    -- publishing, which you can get using @DescribeFunction@.
    ifMatch :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'publishFunction_name' - The name of the function that you are publishing.
--
-- 'ifMatch', 'publishFunction_ifMatch' - The current version (@ETag@ value) of the function that you are
-- publishing, which you can get using @DescribeFunction@.
newPublishFunction ::
  -- | 'name'
  Prelude.Text ->
  -- | 'ifMatch'
  Prelude.Text ->
  PublishFunction
newPublishFunction pName_ pIfMatch_ =
  PublishFunction'
    { name = pName_,
      ifMatch = pIfMatch_
    }

-- | The name of the function that you are publishing.
publishFunction_name :: Lens.Lens' PublishFunction Prelude.Text
publishFunction_name = Lens.lens (\PublishFunction' {name} -> name) (\s@PublishFunction' {} a -> s {name = a} :: PublishFunction)

-- | The current version (@ETag@ value) of the function that you are
-- publishing, which you can get using @DescribeFunction@.
publishFunction_ifMatch :: Lens.Lens' PublishFunction Prelude.Text
publishFunction_ifMatch = Lens.lens (\PublishFunction' {ifMatch} -> ifMatch) (\s@PublishFunction' {} a -> s {ifMatch = a} :: PublishFunction)

instance Core.AWSRequest PublishFunction where
  type
    AWSResponse PublishFunction =
      PublishFunctionResponse
  request overrides =
    Request.post (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          PublishFunctionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PublishFunction where
  hashWithSalt _salt PublishFunction' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ifMatch

instance Prelude.NFData PublishFunction where
  rnf PublishFunction' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf ifMatch

instance Data.ToHeaders PublishFunction where
  toHeaders PublishFunction' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath PublishFunction where
  toPath PublishFunction' {..} =
    Prelude.mconcat
      ["/2020-05-31/function/", Data.toBS name, "/publish"]

instance Data.ToQuery PublishFunction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPublishFunctionResponse' smart constructor.
data PublishFunctionResponse = PublishFunctionResponse'
  { -- | Contains configuration information and metadata about a CloudFront
    -- function.
    functionSummary :: Prelude.Maybe FunctionSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionSummary', 'publishFunctionResponse_functionSummary' - Contains configuration information and metadata about a CloudFront
-- function.
--
-- 'httpStatus', 'publishFunctionResponse_httpStatus' - The response's http status code.
newPublishFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishFunctionResponse
newPublishFunctionResponse pHttpStatus_ =
  PublishFunctionResponse'
    { functionSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains configuration information and metadata about a CloudFront
-- function.
publishFunctionResponse_functionSummary :: Lens.Lens' PublishFunctionResponse (Prelude.Maybe FunctionSummary)
publishFunctionResponse_functionSummary = Lens.lens (\PublishFunctionResponse' {functionSummary} -> functionSummary) (\s@PublishFunctionResponse' {} a -> s {functionSummary = a} :: PublishFunctionResponse)

-- | The response's http status code.
publishFunctionResponse_httpStatus :: Lens.Lens' PublishFunctionResponse Prelude.Int
publishFunctionResponse_httpStatus = Lens.lens (\PublishFunctionResponse' {httpStatus} -> httpStatus) (\s@PublishFunctionResponse' {} a -> s {httpStatus = a} :: PublishFunctionResponse)

instance Prelude.NFData PublishFunctionResponse where
  rnf PublishFunctionResponse' {..} =
    Prelude.rnf functionSummary
      `Prelude.seq` Prelude.rnf httpStatus
