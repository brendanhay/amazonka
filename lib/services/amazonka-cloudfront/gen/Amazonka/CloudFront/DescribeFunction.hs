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
-- Module      : Amazonka.CloudFront.DescribeFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets configuration information and metadata about a CloudFront function,
-- but not the function\'s code. To get a function\'s code, use
-- @GetFunction@.
--
-- To get configuration information and metadata about a function, you must
-- provide the function\'s name and stage. To get these values, you can use
-- @ListFunctions@.
module Amazonka.CloudFront.DescribeFunction
  ( -- * Creating a Request
    DescribeFunction (..),
    newDescribeFunction,

    -- * Request Lenses
    describeFunction_stage,
    describeFunction_name,

    -- * Destructuring the Response
    DescribeFunctionResponse (..),
    newDescribeFunctionResponse,

    -- * Response Lenses
    describeFunctionResponse_eTag,
    describeFunctionResponse_functionSummary,
    describeFunctionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFunction' smart constructor.
data DescribeFunction = DescribeFunction'
  { -- | The function\'s stage, either @DEVELOPMENT@ or @LIVE@.
    stage :: Prelude.Maybe FunctionStage,
    -- | The name of the function that you are getting information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'describeFunction_stage' - The function\'s stage, either @DEVELOPMENT@ or @LIVE@.
--
-- 'name', 'describeFunction_name' - The name of the function that you are getting information about.
newDescribeFunction ::
  -- | 'name'
  Prelude.Text ->
  DescribeFunction
newDescribeFunction pName_ =
  DescribeFunction'
    { stage = Prelude.Nothing,
      name = pName_
    }

-- | The function\'s stage, either @DEVELOPMENT@ or @LIVE@.
describeFunction_stage :: Lens.Lens' DescribeFunction (Prelude.Maybe FunctionStage)
describeFunction_stage = Lens.lens (\DescribeFunction' {stage} -> stage) (\s@DescribeFunction' {} a -> s {stage = a} :: DescribeFunction)

-- | The name of the function that you are getting information about.
describeFunction_name :: Lens.Lens' DescribeFunction Prelude.Text
describeFunction_name = Lens.lens (\DescribeFunction' {name} -> name) (\s@DescribeFunction' {} a -> s {name = a} :: DescribeFunction)

instance Core.AWSRequest DescribeFunction where
  type
    AWSResponse DescribeFunction =
      DescribeFunctionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFunctionResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFunction where
  hashWithSalt _salt DescribeFunction' {..} =
    _salt
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeFunction where
  rnf DescribeFunction' {..} =
    Prelude.rnf stage `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DescribeFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFunction where
  toPath DescribeFunction' {..} =
    Prelude.mconcat
      [ "/2020-05-31/function/",
        Data.toBS name,
        "/describe"
      ]

instance Data.ToQuery DescribeFunction where
  toQuery DescribeFunction' {..} =
    Prelude.mconcat ["Stage" Data.=: stage]

-- | /See:/ 'newDescribeFunctionResponse' smart constructor.
data DescribeFunctionResponse = DescribeFunctionResponse'
  { -- | The version identifier for the current version of the CloudFront
    -- function.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Contains configuration information and metadata about a CloudFront
    -- function.
    functionSummary :: Prelude.Maybe FunctionSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'describeFunctionResponse_eTag' - The version identifier for the current version of the CloudFront
-- function.
--
-- 'functionSummary', 'describeFunctionResponse_functionSummary' - Contains configuration information and metadata about a CloudFront
-- function.
--
-- 'httpStatus', 'describeFunctionResponse_httpStatus' - The response's http status code.
newDescribeFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFunctionResponse
newDescribeFunctionResponse pHttpStatus_ =
  DescribeFunctionResponse'
    { eTag = Prelude.Nothing,
      functionSummary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version identifier for the current version of the CloudFront
-- function.
describeFunctionResponse_eTag :: Lens.Lens' DescribeFunctionResponse (Prelude.Maybe Prelude.Text)
describeFunctionResponse_eTag = Lens.lens (\DescribeFunctionResponse' {eTag} -> eTag) (\s@DescribeFunctionResponse' {} a -> s {eTag = a} :: DescribeFunctionResponse)

-- | Contains configuration information and metadata about a CloudFront
-- function.
describeFunctionResponse_functionSummary :: Lens.Lens' DescribeFunctionResponse (Prelude.Maybe FunctionSummary)
describeFunctionResponse_functionSummary = Lens.lens (\DescribeFunctionResponse' {functionSummary} -> functionSummary) (\s@DescribeFunctionResponse' {} a -> s {functionSummary = a} :: DescribeFunctionResponse)

-- | The response's http status code.
describeFunctionResponse_httpStatus :: Lens.Lens' DescribeFunctionResponse Prelude.Int
describeFunctionResponse_httpStatus = Lens.lens (\DescribeFunctionResponse' {httpStatus} -> httpStatus) (\s@DescribeFunctionResponse' {} a -> s {httpStatus = a} :: DescribeFunctionResponse)

instance Prelude.NFData DescribeFunctionResponse where
  rnf DescribeFunctionResponse' {..} =
    Prelude.rnf eTag `Prelude.seq`
      Prelude.rnf functionSummary `Prelude.seq`
        Prelude.rnf httpStatus
