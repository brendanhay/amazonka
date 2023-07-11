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
-- Module      : Amazonka.CloudFront.UpdateFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a CloudFront function.
--
-- You can update a function\'s code or the comment that describes the
-- function. You cannot update a function\'s name.
--
-- To update a function, you provide the function\'s name and version
-- (@ETag@ value) along with the updated function code. To get the name and
-- version, you can use @ListFunctions@ and @DescribeFunction@.
module Amazonka.CloudFront.UpdateFunction
  ( -- * Creating a Request
    UpdateFunction (..),
    newUpdateFunction,

    -- * Request Lenses
    updateFunction_ifMatch,
    updateFunction_functionConfig,
    updateFunction_functionCode,
    updateFunction_name,

    -- * Destructuring the Response
    UpdateFunctionResponse (..),
    newUpdateFunctionResponse,

    -- * Response Lenses
    updateFunctionResponse_eTag,
    updateFunctionResponse_functionSummary,
    updateFunctionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { -- | The current version (@ETag@ value) of the function that you are
    -- updating, which you can get using @DescribeFunction@.
    ifMatch :: Prelude.Text,
    -- | Configuration information about the function.
    functionConfig :: FunctionConfig,
    -- | The function code. For more information about writing a CloudFront
    -- function, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/writing-function-code.html Writing function code for CloudFront Functions>
    -- in the /Amazon CloudFront Developer Guide/.
    functionCode :: Data.Sensitive Data.Base64,
    -- | The name of the function that you are updating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateFunction_ifMatch' - The current version (@ETag@ value) of the function that you are
-- updating, which you can get using @DescribeFunction@.
--
-- 'functionConfig', 'updateFunction_functionConfig' - Configuration information about the function.
--
-- 'functionCode', 'updateFunction_functionCode' - The function code. For more information about writing a CloudFront
-- function, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/writing-function-code.html Writing function code for CloudFront Functions>
-- in the /Amazon CloudFront Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'name', 'updateFunction_name' - The name of the function that you are updating.
newUpdateFunction ::
  -- | 'ifMatch'
  Prelude.Text ->
  -- | 'functionConfig'
  FunctionConfig ->
  -- | 'functionCode'
  Prelude.ByteString ->
  -- | 'name'
  Prelude.Text ->
  UpdateFunction
newUpdateFunction
  pIfMatch_
  pFunctionConfig_
  pFunctionCode_
  pName_ =
    UpdateFunction'
      { ifMatch = pIfMatch_,
        functionConfig = pFunctionConfig_,
        functionCode =
          Data._Sensitive
            Prelude.. Data._Base64
            Lens.# pFunctionCode_,
        name = pName_
      }

-- | The current version (@ETag@ value) of the function that you are
-- updating, which you can get using @DescribeFunction@.
updateFunction_ifMatch :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_ifMatch = Lens.lens (\UpdateFunction' {ifMatch} -> ifMatch) (\s@UpdateFunction' {} a -> s {ifMatch = a} :: UpdateFunction)

-- | Configuration information about the function.
updateFunction_functionConfig :: Lens.Lens' UpdateFunction FunctionConfig
updateFunction_functionConfig = Lens.lens (\UpdateFunction' {functionConfig} -> functionConfig) (\s@UpdateFunction' {} a -> s {functionConfig = a} :: UpdateFunction)

-- | The function code. For more information about writing a CloudFront
-- function, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/writing-function-code.html Writing function code for CloudFront Functions>
-- in the /Amazon CloudFront Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateFunction_functionCode :: Lens.Lens' UpdateFunction Prelude.ByteString
updateFunction_functionCode = Lens.lens (\UpdateFunction' {functionCode} -> functionCode) (\s@UpdateFunction' {} a -> s {functionCode = a} :: UpdateFunction) Prelude.. Data._Sensitive Prelude.. Data._Base64

-- | The name of the function that you are updating.
updateFunction_name :: Lens.Lens' UpdateFunction Prelude.Text
updateFunction_name = Lens.lens (\UpdateFunction' {name} -> name) (\s@UpdateFunction' {} a -> s {name = a} :: UpdateFunction)

instance Core.AWSRequest UpdateFunction where
  type
    AWSResponse UpdateFunction =
      UpdateFunctionResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateFunctionResponse'
            Prelude.<$> (h Data..#? "ETtag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFunction where
  hashWithSalt _salt UpdateFunction' {..} =
    _salt
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` functionConfig
      `Prelude.hashWithSalt` functionCode
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateFunction where
  rnf UpdateFunction' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf functionConfig
      `Prelude.seq` Prelude.rnf functionCode
      `Prelude.seq` Prelude.rnf name

instance Data.ToElement UpdateFunction where
  toElement =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}UpdateFunctionRequest"

instance Data.ToHeaders UpdateFunction where
  toHeaders UpdateFunction' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateFunction where
  toPath UpdateFunction' {..} =
    Prelude.mconcat
      ["/2020-05-31/function/", Data.toBS name]

instance Data.ToQuery UpdateFunction where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML UpdateFunction where
  toXML UpdateFunction' {..} =
    Prelude.mconcat
      [ "FunctionConfig" Data.@= functionConfig,
        "FunctionCode" Data.@= functionCode
      ]

-- | /See:/ 'newUpdateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
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
-- Create a value of 'UpdateFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateFunctionResponse_eTag' - The version identifier for the current version of the CloudFront
-- function.
--
-- 'functionSummary', 'updateFunctionResponse_functionSummary' - Contains configuration information and metadata about a CloudFront
-- function.
--
-- 'httpStatus', 'updateFunctionResponse_httpStatus' - The response's http status code.
newUpdateFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFunctionResponse
newUpdateFunctionResponse pHttpStatus_ =
  UpdateFunctionResponse'
    { eTag = Prelude.Nothing,
      functionSummary = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version identifier for the current version of the CloudFront
-- function.
updateFunctionResponse_eTag :: Lens.Lens' UpdateFunctionResponse (Prelude.Maybe Prelude.Text)
updateFunctionResponse_eTag = Lens.lens (\UpdateFunctionResponse' {eTag} -> eTag) (\s@UpdateFunctionResponse' {} a -> s {eTag = a} :: UpdateFunctionResponse)

-- | Contains configuration information and metadata about a CloudFront
-- function.
updateFunctionResponse_functionSummary :: Lens.Lens' UpdateFunctionResponse (Prelude.Maybe FunctionSummary)
updateFunctionResponse_functionSummary = Lens.lens (\UpdateFunctionResponse' {functionSummary} -> functionSummary) (\s@UpdateFunctionResponse' {} a -> s {functionSummary = a} :: UpdateFunctionResponse)

-- | The response's http status code.
updateFunctionResponse_httpStatus :: Lens.Lens' UpdateFunctionResponse Prelude.Int
updateFunctionResponse_httpStatus = Lens.lens (\UpdateFunctionResponse' {httpStatus} -> httpStatus) (\s@UpdateFunctionResponse' {} a -> s {httpStatus = a} :: UpdateFunctionResponse)

instance Prelude.NFData UpdateFunctionResponse where
  rnf UpdateFunctionResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf functionSummary
      `Prelude.seq` Prelude.rnf httpStatus
