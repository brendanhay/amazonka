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
-- Module      : Amazonka.CloudFront.CreateFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CloudFront function.
--
-- To create a function, you provide the function code and some
-- configuration information about the function. The response contains an
-- Amazon Resource Name (ARN) that uniquely identifies the function.
--
-- When you create a function, it\'s in the @DEVELOPMENT@ stage. In this
-- stage, you can test the function with @TestFunction@, and update it with
-- @UpdateFunction@.
--
-- When you\'re ready to use your function with a CloudFront distribution,
-- use @PublishFunction@ to copy the function from the @DEVELOPMENT@ stage
-- to @LIVE@. When it\'s live, you can attach the function to a
-- distribution\'s cache behavior, using the function\'s ARN.
module Amazonka.CloudFront.CreateFunction
  ( -- * Creating a Request
    CreateFunction (..),
    newCreateFunction,

    -- * Request Lenses
    createFunction_name,
    createFunction_functionConfig,
    createFunction_functionCode,

    -- * Destructuring the Response
    CreateFunctionResponse (..),
    newCreateFunctionResponse,

    -- * Response Lenses
    createFunctionResponse_eTag,
    createFunctionResponse_functionSummary,
    createFunctionResponse_location,
    createFunctionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFunction' smart constructor.
data CreateFunction = CreateFunction'
  { -- | A name to identify the function.
    name :: Prelude.Text,
    -- | Configuration information about the function, including an optional
    -- comment and the function\'s runtime.
    functionConfig :: FunctionConfig,
    -- | The function code. For more information about writing a CloudFront
    -- function, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/writing-function-code.html Writing function code for CloudFront Functions>
    -- in the /Amazon CloudFront Developer Guide/.
    functionCode :: Data.Sensitive Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createFunction_name' - A name to identify the function.
--
-- 'functionConfig', 'createFunction_functionConfig' - Configuration information about the function, including an optional
-- comment and the function\'s runtime.
--
-- 'functionCode', 'createFunction_functionCode' - The function code. For more information about writing a CloudFront
-- function, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/writing-function-code.html Writing function code for CloudFront Functions>
-- in the /Amazon CloudFront Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newCreateFunction ::
  -- | 'name'
  Prelude.Text ->
  -- | 'functionConfig'
  FunctionConfig ->
  -- | 'functionCode'
  Prelude.ByteString ->
  CreateFunction
newCreateFunction
  pName_
  pFunctionConfig_
  pFunctionCode_ =
    CreateFunction'
      { name = pName_,
        functionConfig = pFunctionConfig_,
        functionCode =
          Data._Sensitive
            Prelude.. Data._Base64
            Lens.# pFunctionCode_
      }

-- | A name to identify the function.
createFunction_name :: Lens.Lens' CreateFunction Prelude.Text
createFunction_name = Lens.lens (\CreateFunction' {name} -> name) (\s@CreateFunction' {} a -> s {name = a} :: CreateFunction)

-- | Configuration information about the function, including an optional
-- comment and the function\'s runtime.
createFunction_functionConfig :: Lens.Lens' CreateFunction FunctionConfig
createFunction_functionConfig = Lens.lens (\CreateFunction' {functionConfig} -> functionConfig) (\s@CreateFunction' {} a -> s {functionConfig = a} :: CreateFunction)

-- | The function code. For more information about writing a CloudFront
-- function, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/writing-function-code.html Writing function code for CloudFront Functions>
-- in the /Amazon CloudFront Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createFunction_functionCode :: Lens.Lens' CreateFunction Prelude.ByteString
createFunction_functionCode = Lens.lens (\CreateFunction' {functionCode} -> functionCode) (\s@CreateFunction' {} a -> s {functionCode = a} :: CreateFunction) Prelude.. Data._Sensitive Prelude.. Data._Base64

instance Core.AWSRequest CreateFunction where
  type
    AWSResponse CreateFunction =
      CreateFunctionResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFunctionResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFunction where
  hashWithSalt _salt CreateFunction' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` functionConfig
      `Prelude.hashWithSalt` functionCode

instance Prelude.NFData CreateFunction where
  rnf CreateFunction' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf functionConfig
      `Prelude.seq` Prelude.rnf functionCode

instance Data.ToElement CreateFunction where
  toElement =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CreateFunctionRequest"

instance Data.ToHeaders CreateFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateFunction where
  toPath = Prelude.const "/2020-05-31/function"

instance Data.ToQuery CreateFunction where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML CreateFunction where
  toXML CreateFunction' {..} =
    Prelude.mconcat
      [ "Name" Data.@= name,
        "FunctionConfig" Data.@= functionConfig,
        "FunctionCode" Data.@= functionCode
      ]

-- | /See:/ 'newCreateFunctionResponse' smart constructor.
data CreateFunctionResponse = CreateFunctionResponse'
  { -- | The version identifier for the current version of the CloudFront
    -- function.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Contains configuration information and metadata about a CloudFront
    -- function.
    functionSummary :: Prelude.Maybe FunctionSummary,
    -- | The URL of the CloudFront function. Use the URL to manage the function
    -- with the CloudFront API.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createFunctionResponse_eTag' - The version identifier for the current version of the CloudFront
-- function.
--
-- 'functionSummary', 'createFunctionResponse_functionSummary' - Contains configuration information and metadata about a CloudFront
-- function.
--
-- 'location', 'createFunctionResponse_location' - The URL of the CloudFront function. Use the URL to manage the function
-- with the CloudFront API.
--
-- 'httpStatus', 'createFunctionResponse_httpStatus' - The response's http status code.
newCreateFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFunctionResponse
newCreateFunctionResponse pHttpStatus_ =
  CreateFunctionResponse'
    { eTag = Prelude.Nothing,
      functionSummary = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version identifier for the current version of the CloudFront
-- function.
createFunctionResponse_eTag :: Lens.Lens' CreateFunctionResponse (Prelude.Maybe Prelude.Text)
createFunctionResponse_eTag = Lens.lens (\CreateFunctionResponse' {eTag} -> eTag) (\s@CreateFunctionResponse' {} a -> s {eTag = a} :: CreateFunctionResponse)

-- | Contains configuration information and metadata about a CloudFront
-- function.
createFunctionResponse_functionSummary :: Lens.Lens' CreateFunctionResponse (Prelude.Maybe FunctionSummary)
createFunctionResponse_functionSummary = Lens.lens (\CreateFunctionResponse' {functionSummary} -> functionSummary) (\s@CreateFunctionResponse' {} a -> s {functionSummary = a} :: CreateFunctionResponse)

-- | The URL of the CloudFront function. Use the URL to manage the function
-- with the CloudFront API.
createFunctionResponse_location :: Lens.Lens' CreateFunctionResponse (Prelude.Maybe Prelude.Text)
createFunctionResponse_location = Lens.lens (\CreateFunctionResponse' {location} -> location) (\s@CreateFunctionResponse' {} a -> s {location = a} :: CreateFunctionResponse)

-- | The response's http status code.
createFunctionResponse_httpStatus :: Lens.Lens' CreateFunctionResponse Prelude.Int
createFunctionResponse_httpStatus = Lens.lens (\CreateFunctionResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionResponse' {} a -> s {httpStatus = a} :: CreateFunctionResponse)

instance Prelude.NFData CreateFunctionResponse where
  rnf CreateFunctionResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf functionSummary
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
