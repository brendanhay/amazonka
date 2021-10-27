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
-- Module      : Network.AWS.Kendra.DescribeDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Amazon Kendra data source.
module Network.AWS.Kendra.DescribeDataSource
  ( -- * Creating a Request
    DescribeDataSource (..),
    newDescribeDataSource,

    -- * Request Lenses
    describeDataSource_id,
    describeDataSource_indexId,

    -- * Destructuring the Response
    DescribeDataSourceResponse (..),
    newDescribeDataSourceResponse,

    -- * Response Lenses
    describeDataSourceResponse_status,
    describeDataSourceResponse_languageCode,
    describeDataSourceResponse_createdAt,
    describeDataSourceResponse_schedule,
    describeDataSourceResponse_name,
    describeDataSourceResponse_id,
    describeDataSourceResponse_configuration,
    describeDataSourceResponse_type,
    describeDataSourceResponse_updatedAt,
    describeDataSourceResponse_errorMessage,
    describeDataSourceResponse_indexId,
    describeDataSourceResponse_description,
    describeDataSourceResponse_roleArn,
    describeDataSourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDataSource' smart constructor.
data DescribeDataSource = DescribeDataSource'
  { -- | The unique identifier of the data source to describe.
    id :: Prelude.Text,
    -- | The identifier of the index that contains the data source.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeDataSource_id' - The unique identifier of the data source to describe.
--
-- 'indexId', 'describeDataSource_indexId' - The identifier of the index that contains the data source.
newDescribeDataSource ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DescribeDataSource
newDescribeDataSource pId_ pIndexId_ =
  DescribeDataSource' {id = pId_, indexId = pIndexId_}

-- | The unique identifier of the data source to describe.
describeDataSource_id :: Lens.Lens' DescribeDataSource Prelude.Text
describeDataSource_id = Lens.lens (\DescribeDataSource' {id} -> id) (\s@DescribeDataSource' {} a -> s {id = a} :: DescribeDataSource)

-- | The identifier of the index that contains the data source.
describeDataSource_indexId :: Lens.Lens' DescribeDataSource Prelude.Text
describeDataSource_indexId = Lens.lens (\DescribeDataSource' {indexId} -> indexId) (\s@DescribeDataSource' {} a -> s {indexId = a} :: DescribeDataSource)

instance Core.AWSRequest DescribeDataSource where
  type
    AWSResponse DescribeDataSource =
      DescribeDataSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDataSourceResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "Schedule")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Configuration")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "IndexId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDataSource

instance Prelude.NFData DescribeDataSource

instance Core.ToHeaders DescribeDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeDataSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDataSource where
  toJSON DescribeDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath DescribeDataSource where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDataSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDataSourceResponse' smart constructor.
data DescribeDataSourceResponse = DescribeDataSourceResponse'
  { -- | The current status of the data source. When the status is @ACTIVE@ the
    -- data source is ready to use. When the status is @FAILED@, the
    -- @ErrorMessage@ field contains the reason that the data source failed.
    status :: Prelude.Maybe DataSourceStatus,
    -- | The code for a language. This shows a supported language for all
    -- documents in the data source. English is supported by default. For more
    -- information on supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp of when the data source was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The schedule that Amazon Kendra will update the data source.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name that you gave the data source when it was created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the data source.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information that describes where the data source is located and how the
    -- data source is configured. The specific information in the description
    -- depends on the data source provider.
    configuration :: Prelude.Maybe DataSourceConfiguration,
    -- | The type of the data source.
    type' :: Prelude.Maybe DataSourceType,
    -- | The Unix timestamp of when the data source was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- contains a description of the error that caused the data source to fail.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index that contains the data source.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | The description of the data source.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role that enables the data source
    -- to access its resources.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeDataSourceResponse_status' - The current status of the data source. When the status is @ACTIVE@ the
-- data source is ready to use. When the status is @FAILED@, the
-- @ErrorMessage@ field contains the reason that the data source failed.
--
-- 'languageCode', 'describeDataSourceResponse_languageCode' - The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'createdAt', 'describeDataSourceResponse_createdAt' - The Unix timestamp of when the data source was created.
--
-- 'schedule', 'describeDataSourceResponse_schedule' - The schedule that Amazon Kendra will update the data source.
--
-- 'name', 'describeDataSourceResponse_name' - The name that you gave the data source when it was created.
--
-- 'id', 'describeDataSourceResponse_id' - The identifier of the data source.
--
-- 'configuration', 'describeDataSourceResponse_configuration' - Information that describes where the data source is located and how the
-- data source is configured. The specific information in the description
-- depends on the data source provider.
--
-- 'type'', 'describeDataSourceResponse_type' - The type of the data source.
--
-- 'updatedAt', 'describeDataSourceResponse_updatedAt' - The Unix timestamp of when the data source was last updated.
--
-- 'errorMessage', 'describeDataSourceResponse_errorMessage' - When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a description of the error that caused the data source to fail.
--
-- 'indexId', 'describeDataSourceResponse_indexId' - The identifier of the index that contains the data source.
--
-- 'description', 'describeDataSourceResponse_description' - The description of the data source.
--
-- 'roleArn', 'describeDataSourceResponse_roleArn' - The Amazon Resource Name (ARN) of the role that enables the data source
-- to access its resources.
--
-- 'httpStatus', 'describeDataSourceResponse_httpStatus' - The response's http status code.
newDescribeDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataSourceResponse
newDescribeDataSourceResponse pHttpStatus_ =
  DescribeDataSourceResponse'
    { status =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      schedule = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      configuration = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      indexId = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the data source. When the status is @ACTIVE@ the
-- data source is ready to use. When the status is @FAILED@, the
-- @ErrorMessage@ field contains the reason that the data source failed.
describeDataSourceResponse_status :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceStatus)
describeDataSourceResponse_status = Lens.lens (\DescribeDataSourceResponse' {status} -> status) (\s@DescribeDataSourceResponse' {} a -> s {status = a} :: DescribeDataSourceResponse)

-- | The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
describeDataSourceResponse_languageCode :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_languageCode = Lens.lens (\DescribeDataSourceResponse' {languageCode} -> languageCode) (\s@DescribeDataSourceResponse' {} a -> s {languageCode = a} :: DescribeDataSourceResponse)

-- | The Unix timestamp of when the data source was created.
describeDataSourceResponse_createdAt :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeDataSourceResponse_createdAt = Lens.lens (\DescribeDataSourceResponse' {createdAt} -> createdAt) (\s@DescribeDataSourceResponse' {} a -> s {createdAt = a} :: DescribeDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The schedule that Amazon Kendra will update the data source.
describeDataSourceResponse_schedule :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_schedule = Lens.lens (\DescribeDataSourceResponse' {schedule} -> schedule) (\s@DescribeDataSourceResponse' {} a -> s {schedule = a} :: DescribeDataSourceResponse)

-- | The name that you gave the data source when it was created.
describeDataSourceResponse_name :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_name = Lens.lens (\DescribeDataSourceResponse' {name} -> name) (\s@DescribeDataSourceResponse' {} a -> s {name = a} :: DescribeDataSourceResponse)

-- | The identifier of the data source.
describeDataSourceResponse_id :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_id = Lens.lens (\DescribeDataSourceResponse' {id} -> id) (\s@DescribeDataSourceResponse' {} a -> s {id = a} :: DescribeDataSourceResponse)

-- | Information that describes where the data source is located and how the
-- data source is configured. The specific information in the description
-- depends on the data source provider.
describeDataSourceResponse_configuration :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceConfiguration)
describeDataSourceResponse_configuration = Lens.lens (\DescribeDataSourceResponse' {configuration} -> configuration) (\s@DescribeDataSourceResponse' {} a -> s {configuration = a} :: DescribeDataSourceResponse)

-- | The type of the data source.
describeDataSourceResponse_type :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe DataSourceType)
describeDataSourceResponse_type = Lens.lens (\DescribeDataSourceResponse' {type'} -> type') (\s@DescribeDataSourceResponse' {} a -> s {type' = a} :: DescribeDataSourceResponse)

-- | The Unix timestamp of when the data source was last updated.
describeDataSourceResponse_updatedAt :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeDataSourceResponse_updatedAt = Lens.lens (\DescribeDataSourceResponse' {updatedAt} -> updatedAt) (\s@DescribeDataSourceResponse' {} a -> s {updatedAt = a} :: DescribeDataSourceResponse) Prelude.. Lens.mapping Core._Time

-- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a description of the error that caused the data source to fail.
describeDataSourceResponse_errorMessage :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_errorMessage = Lens.lens (\DescribeDataSourceResponse' {errorMessage} -> errorMessage) (\s@DescribeDataSourceResponse' {} a -> s {errorMessage = a} :: DescribeDataSourceResponse)

-- | The identifier of the index that contains the data source.
describeDataSourceResponse_indexId :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_indexId = Lens.lens (\DescribeDataSourceResponse' {indexId} -> indexId) (\s@DescribeDataSourceResponse' {} a -> s {indexId = a} :: DescribeDataSourceResponse)

-- | The description of the data source.
describeDataSourceResponse_description :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_description = Lens.lens (\DescribeDataSourceResponse' {description} -> description) (\s@DescribeDataSourceResponse' {} a -> s {description = a} :: DescribeDataSourceResponse)

-- | The Amazon Resource Name (ARN) of the role that enables the data source
-- to access its resources.
describeDataSourceResponse_roleArn :: Lens.Lens' DescribeDataSourceResponse (Prelude.Maybe Prelude.Text)
describeDataSourceResponse_roleArn = Lens.lens (\DescribeDataSourceResponse' {roleArn} -> roleArn) (\s@DescribeDataSourceResponse' {} a -> s {roleArn = a} :: DescribeDataSourceResponse)

-- | The response's http status code.
describeDataSourceResponse_httpStatus :: Lens.Lens' DescribeDataSourceResponse Prelude.Int
describeDataSourceResponse_httpStatus = Lens.lens (\DescribeDataSourceResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSourceResponse' {} a -> s {httpStatus = a} :: DescribeDataSourceResponse)

instance Prelude.NFData DescribeDataSourceResponse
