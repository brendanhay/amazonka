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
-- Module      : Amazonka.CloudFront.TestFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a CloudFront function.
--
-- To test a function, you provide an /event object/ that represents an
-- HTTP request or response that your CloudFront distribution could receive
-- in production. CloudFront runs the function, passing it the event object
-- that you provided, and returns the function’s result (the modified event
-- object) in the response. The response also contains function logs and
-- error messages, if any exist. For more information about testing
-- functions, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/managing-functions.html#test-function Testing functions>
-- in the /Amazon CloudFront Developer Guide/.
--
-- To test a function, you provide the function’s name and version (@ETag@
-- value) along with the event object. To get the function’s name and
-- version, you can use @ListFunctions@ and @DescribeFunction@.
module Amazonka.CloudFront.TestFunction
  ( -- * Creating a Request
    TestFunction (..),
    newTestFunction,

    -- * Request Lenses
    testFunction_stage,
    testFunction_name,
    testFunction_ifMatch,
    testFunction_eventObject,

    -- * Destructuring the Response
    TestFunctionResponse (..),
    newTestFunctionResponse,

    -- * Response Lenses
    testFunctionResponse_testResult,
    testFunctionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestFunction' smart constructor.
data TestFunction = TestFunction'
  { -- | The stage of the function that you are testing, either @DEVELOPMENT@ or
    -- @LIVE@.
    stage :: Prelude.Maybe FunctionStage,
    -- | The name of the function that you are testing.
    name :: Prelude.Text,
    -- | The current version (@ETag@ value) of the function that you are testing,
    -- which you can get using @DescribeFunction@.
    ifMatch :: Prelude.Text,
    -- | The event object to test the function with. For more information about
    -- the structure of the event object, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/managing-functions.html#test-function Testing functions>
    -- in the /Amazon CloudFront Developer Guide/.
    eventObject :: Data.Sensitive Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'testFunction_stage' - The stage of the function that you are testing, either @DEVELOPMENT@ or
-- @LIVE@.
--
-- 'name', 'testFunction_name' - The name of the function that you are testing.
--
-- 'ifMatch', 'testFunction_ifMatch' - The current version (@ETag@ value) of the function that you are testing,
-- which you can get using @DescribeFunction@.
--
-- 'eventObject', 'testFunction_eventObject' - The event object to test the function with. For more information about
-- the structure of the event object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/managing-functions.html#test-function Testing functions>
-- in the /Amazon CloudFront Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newTestFunction ::
  -- | 'name'
  Prelude.Text ->
  -- | 'ifMatch'
  Prelude.Text ->
  -- | 'eventObject'
  Prelude.ByteString ->
  TestFunction
newTestFunction pName_ pIfMatch_ pEventObject_ =
  TestFunction'
    { stage = Prelude.Nothing,
      name = pName_,
      ifMatch = pIfMatch_,
      eventObject =
        Data._Sensitive Prelude.. Data._Base64
          Lens.# pEventObject_
    }

-- | The stage of the function that you are testing, either @DEVELOPMENT@ or
-- @LIVE@.
testFunction_stage :: Lens.Lens' TestFunction (Prelude.Maybe FunctionStage)
testFunction_stage = Lens.lens (\TestFunction' {stage} -> stage) (\s@TestFunction' {} a -> s {stage = a} :: TestFunction)

-- | The name of the function that you are testing.
testFunction_name :: Lens.Lens' TestFunction Prelude.Text
testFunction_name = Lens.lens (\TestFunction' {name} -> name) (\s@TestFunction' {} a -> s {name = a} :: TestFunction)

-- | The current version (@ETag@ value) of the function that you are testing,
-- which you can get using @DescribeFunction@.
testFunction_ifMatch :: Lens.Lens' TestFunction Prelude.Text
testFunction_ifMatch = Lens.lens (\TestFunction' {ifMatch} -> ifMatch) (\s@TestFunction' {} a -> s {ifMatch = a} :: TestFunction)

-- | The event object to test the function with. For more information about
-- the structure of the event object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/managing-functions.html#test-function Testing functions>
-- in the /Amazon CloudFront Developer Guide/.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
testFunction_eventObject :: Lens.Lens' TestFunction Prelude.ByteString
testFunction_eventObject = Lens.lens (\TestFunction' {eventObject} -> eventObject) (\s@TestFunction' {} a -> s {eventObject = a} :: TestFunction) Prelude.. Data._Sensitive Prelude.. Data._Base64

instance Core.AWSRequest TestFunction where
  type AWSResponse TestFunction = TestFunctionResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          TestFunctionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestFunction where
  hashWithSalt _salt TestFunction' {..} =
    _salt `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` eventObject

instance Prelude.NFData TestFunction where
  rnf TestFunction' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf eventObject

instance Data.ToElement TestFunction where
  toElement =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}TestFunctionRequest"

instance Data.ToHeaders TestFunction where
  toHeaders TestFunction' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath TestFunction where
  toPath TestFunction' {..} =
    Prelude.mconcat
      ["/2020-05-31/function/", Data.toBS name, "/test"]

instance Data.ToQuery TestFunction where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML TestFunction where
  toXML TestFunction' {..} =
    Prelude.mconcat
      [ "Stage" Data.@= stage,
        "EventObject" Data.@= eventObject
      ]

-- | /See:/ 'newTestFunctionResponse' smart constructor.
data TestFunctionResponse = TestFunctionResponse'
  { -- | An object that represents the result of running the function with the
    -- provided event object.
    testResult :: Prelude.Maybe TestResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testResult', 'testFunctionResponse_testResult' - An object that represents the result of running the function with the
-- provided event object.
--
-- 'httpStatus', 'testFunctionResponse_httpStatus' - The response's http status code.
newTestFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestFunctionResponse
newTestFunctionResponse pHttpStatus_ =
  TestFunctionResponse'
    { testResult = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that represents the result of running the function with the
-- provided event object.
testFunctionResponse_testResult :: Lens.Lens' TestFunctionResponse (Prelude.Maybe TestResult)
testFunctionResponse_testResult = Lens.lens (\TestFunctionResponse' {testResult} -> testResult) (\s@TestFunctionResponse' {} a -> s {testResult = a} :: TestFunctionResponse)

-- | The response's http status code.
testFunctionResponse_httpStatus :: Lens.Lens' TestFunctionResponse Prelude.Int
testFunctionResponse_httpStatus = Lens.lens (\TestFunctionResponse' {httpStatus} -> httpStatus) (\s@TestFunctionResponse' {} a -> s {httpStatus = a} :: TestFunctionResponse)

instance Prelude.NFData TestFunctionResponse where
  rnf TestFunctionResponse' {..} =
    Prelude.rnf testResult
      `Prelude.seq` Prelude.rnf httpStatus
