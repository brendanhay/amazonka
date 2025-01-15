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
-- Module      : Amazonka.CloudFormation.TestType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a registered extension to make sure it meets all necessary
-- requirements for being published in the CloudFormation registry.
--
-- -   For resource types, this includes passing all contracts tests
--     defined for the type.
--
-- -   For modules, this includes determining if the module\'s model meets
--     all necessary requirements.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html#publish-extension-testing Testing your public extension prior to publishing>
-- in the /CloudFormation CLI User Guide/.
--
-- If you don\'t specify a version, CloudFormation uses the default version
-- of the extension in your account and region for testing.
--
-- To perform testing, CloudFormation assumes the execution role specified
-- when the type was registered. For more information, see
-- <AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- Once you\'ve initiated testing on an extension using @TestType@, you can
-- pass the returned @TypeVersionArn@ into
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeType.html DescribeType>
-- to monitor the current test status and test status description for the
-- extension.
--
-- An extension must have a test status of @PASSED@ before it can be
-- published. For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-publish.html Publishing extensions to make them available for public use>
-- in the /CloudFormation CLI User Guide/.
module Amazonka.CloudFormation.TestType
  ( -- * Creating a Request
    TestType (..),
    newTestType,

    -- * Request Lenses
    testType_arn,
    testType_logDeliveryBucket,
    testType_type,
    testType_typeName,
    testType_versionId,

    -- * Destructuring the Response
    TestTypeResponse (..),
    newTestTypeResponse,

    -- * Response Lenses
    testTypeResponse_typeVersionArn,
    testTypeResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestType' smart constructor.
data TestType = TestType'
  { -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket to which CloudFormation delivers the contract test
    -- execution logs.
    --
    -- CloudFormation delivers the logs by the time contract testing has
    -- completed and the extension has been assigned a test type status of
    -- @PASSED@ or @FAILED@.
    --
    -- The user calling @TestType@ must be able to access items in the
    -- specified S3 bucket. Specifically, the user needs the following
    -- permissions:
    --
    -- -   @GetObject@
    --
    -- -   @PutObject@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
    -- in the /Amazon Web Services Identity and Access Management User Guide/.
    logDeliveryBucket :: Prelude.Maybe Prelude.Text,
    -- | The type of the extension to test.
    --
    -- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
    type' :: Prelude.Maybe ThirdPartyType,
    -- | The name of the extension to test.
    --
    -- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The version of the extension to test.
    --
    -- You can specify the version id with either @Arn@, or with @TypeName@ and
    -- @Type@.
    --
    -- If you don\'t specify a version, CloudFormation uses the default version
    -- of the extension in this account and region for testing.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'testType_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
--
-- 'logDeliveryBucket', 'testType_logDeliveryBucket' - The S3 bucket to which CloudFormation delivers the contract test
-- execution logs.
--
-- CloudFormation delivers the logs by the time contract testing has
-- completed and the extension has been assigned a test type status of
-- @PASSED@ or @FAILED@.
--
-- The user calling @TestType@ must be able to access items in the
-- specified S3 bucket. Specifically, the user needs the following
-- permissions:
--
-- -   @GetObject@
--
-- -   @PutObject@
--
-- For more information, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
--
-- 'type'', 'testType_type' - The type of the extension to test.
--
-- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
--
-- 'typeName', 'testType_typeName' - The name of the extension to test.
--
-- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
--
-- 'versionId', 'testType_versionId' - The version of the extension to test.
--
-- You can specify the version id with either @Arn@, or with @TypeName@ and
-- @Type@.
--
-- If you don\'t specify a version, CloudFormation uses the default version
-- of the extension in this account and region for testing.
newTestType ::
  TestType
newTestType =
  TestType'
    { arn = Prelude.Nothing,
      logDeliveryBucket = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeName = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
testType_arn :: Lens.Lens' TestType (Prelude.Maybe Prelude.Text)
testType_arn = Lens.lens (\TestType' {arn} -> arn) (\s@TestType' {} a -> s {arn = a} :: TestType)

-- | The S3 bucket to which CloudFormation delivers the contract test
-- execution logs.
--
-- CloudFormation delivers the logs by the time contract testing has
-- completed and the extension has been assigned a test type status of
-- @PASSED@ or @FAILED@.
--
-- The user calling @TestType@ must be able to access items in the
-- specified S3 bucket. Specifically, the user needs the following
-- permissions:
--
-- -   @GetObject@
--
-- -   @PutObject@
--
-- For more information, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
-- in the /Amazon Web Services Identity and Access Management User Guide/.
testType_logDeliveryBucket :: Lens.Lens' TestType (Prelude.Maybe Prelude.Text)
testType_logDeliveryBucket = Lens.lens (\TestType' {logDeliveryBucket} -> logDeliveryBucket) (\s@TestType' {} a -> s {logDeliveryBucket = a} :: TestType)

-- | The type of the extension to test.
--
-- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
testType_type :: Lens.Lens' TestType (Prelude.Maybe ThirdPartyType)
testType_type = Lens.lens (\TestType' {type'} -> type') (\s@TestType' {} a -> s {type' = a} :: TestType)

-- | The name of the extension to test.
--
-- Conditional: You must specify @Arn@, or @TypeName@ and @Type@.
testType_typeName :: Lens.Lens' TestType (Prelude.Maybe Prelude.Text)
testType_typeName = Lens.lens (\TestType' {typeName} -> typeName) (\s@TestType' {} a -> s {typeName = a} :: TestType)

-- | The version of the extension to test.
--
-- You can specify the version id with either @Arn@, or with @TypeName@ and
-- @Type@.
--
-- If you don\'t specify a version, CloudFormation uses the default version
-- of the extension in this account and region for testing.
testType_versionId :: Lens.Lens' TestType (Prelude.Maybe Prelude.Text)
testType_versionId = Lens.lens (\TestType' {versionId} -> versionId) (\s@TestType' {} a -> s {versionId = a} :: TestType)

instance Core.AWSRequest TestType where
  type AWSResponse TestType = TestTypeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "TestTypeResult"
      ( \s h x ->
          TestTypeResponse'
            Prelude.<$> (x Data..@? "TypeVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestType where
  hashWithSalt _salt TestType' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` logDeliveryBucket
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData TestType where
  rnf TestType' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf logDeliveryBucket `Prelude.seq`
        Prelude.rnf type' `Prelude.seq`
          Prelude.rnf typeName `Prelude.seq`
            Prelude.rnf versionId

instance Data.ToHeaders TestType where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TestType where
  toPath = Prelude.const "/"

instance Data.ToQuery TestType where
  toQuery TestType' {..} =
    Prelude.mconcat
      [ "Action" Data.=: ("TestType" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Arn" Data.=: arn,
        "LogDeliveryBucket" Data.=: logDeliveryBucket,
        "Type" Data.=: type',
        "TypeName" Data.=: typeName,
        "VersionId" Data.=: versionId
      ]

-- | /See:/ 'newTestTypeResponse' smart constructor.
data TestTypeResponse = TestTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the extension.
    typeVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeVersionArn', 'testTypeResponse_typeVersionArn' - The Amazon Resource Name (ARN) of the extension.
--
-- 'httpStatus', 'testTypeResponse_httpStatus' - The response's http status code.
newTestTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestTypeResponse
newTestTypeResponse pHttpStatus_ =
  TestTypeResponse'
    { typeVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the extension.
testTypeResponse_typeVersionArn :: Lens.Lens' TestTypeResponse (Prelude.Maybe Prelude.Text)
testTypeResponse_typeVersionArn = Lens.lens (\TestTypeResponse' {typeVersionArn} -> typeVersionArn) (\s@TestTypeResponse' {} a -> s {typeVersionArn = a} :: TestTypeResponse)

-- | The response's http status code.
testTypeResponse_httpStatus :: Lens.Lens' TestTypeResponse Prelude.Int
testTypeResponse_httpStatus = Lens.lens (\TestTypeResponse' {httpStatus} -> httpStatus) (\s@TestTypeResponse' {} a -> s {httpStatus = a} :: TestTypeResponse)

instance Prelude.NFData TestTypeResponse where
  rnf TestTypeResponse' {..} =
    Prelude.rnf typeVersionArn `Prelude.seq`
      Prelude.rnf httpStatus
