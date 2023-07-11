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
-- Module      : Amazonka.Kendra.DescribeIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an existing Amazon Kendra index.
module Amazonka.Kendra.DescribeIndex
  ( -- * Creating a Request
    DescribeIndex (..),
    newDescribeIndex,

    -- * Request Lenses
    describeIndex_id,

    -- * Destructuring the Response
    DescribeIndexResponse (..),
    newDescribeIndexResponse,

    -- * Response Lenses
    describeIndexResponse_capacityUnits,
    describeIndexResponse_createdAt,
    describeIndexResponse_description,
    describeIndexResponse_documentMetadataConfigurations,
    describeIndexResponse_edition,
    describeIndexResponse_errorMessage,
    describeIndexResponse_id,
    describeIndexResponse_indexStatistics,
    describeIndexResponse_name,
    describeIndexResponse_roleArn,
    describeIndexResponse_serverSideEncryptionConfiguration,
    describeIndexResponse_status,
    describeIndexResponse_updatedAt,
    describeIndexResponse_userContextPolicy,
    describeIndexResponse_userGroupResolutionConfiguration,
    describeIndexResponse_userTokenConfigurations,
    describeIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIndex' smart constructor.
data DescribeIndex = DescribeIndex'
  { -- | The identifier of the index you want to get information on.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeIndex_id' - The identifier of the index you want to get information on.
newDescribeIndex ::
  -- | 'id'
  Prelude.Text ->
  DescribeIndex
newDescribeIndex pId_ = DescribeIndex' {id = pId_}

-- | The identifier of the index you want to get information on.
describeIndex_id :: Lens.Lens' DescribeIndex Prelude.Text
describeIndex_id = Lens.lens (\DescribeIndex' {id} -> id) (\s@DescribeIndex' {} a -> s {id = a} :: DescribeIndex)

instance Core.AWSRequest DescribeIndex where
  type
    AWSResponse DescribeIndex =
      DescribeIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            Prelude.<$> (x Data..?> "CapacityUnits")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> ( x
                            Data..?> "DocumentMetadataConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Edition")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "IndexStatistics")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (x Data..?> "UserContextPolicy")
            Prelude.<*> (x Data..?> "UserGroupResolutionConfiguration")
            Prelude.<*> ( x
                            Data..?> "UserTokenConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIndex where
  hashWithSalt _salt DescribeIndex' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeIndex where
  rnf DescribeIndex' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribeIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeIndex where
  toJSON DescribeIndex' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("Id" Data..= id)])

instance Data.ToPath DescribeIndex where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { -- | For Enterprise Edition indexes, you can choose to use additional
    -- capacity to meet the needs of your application. This contains the
    -- capacity units used for the index. A query or document storage capacity
    -- of zero indicates that the index is using the default capacity. For more
    -- information on the default capacity for an index and adjusting this, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | The Unix datetime that the index was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description for the index.
    description :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for document metadata or fields. Document
    -- metadata are fields or attributes associated with your documents. For
    -- example, the company department name associated with each document.
    documentMetadataConfigurations :: Prelude.Maybe [DocumentMetadataConfiguration],
    -- | The Amazon Kendra edition used for the index. You decide the edition
    -- when you create the index.
    edition :: Prelude.Maybe IndexEdition,
    -- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- contains a message that explains why.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index.
    id :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the number of FAQ questions and answers and
    -- the number of text documents indexed.
    indexStatistics :: Prelude.Maybe IndexStatistics,
    -- | The name of the index.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
    -- permission to write to your Amazon Cloudwatch logs.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMScustomer master key (CMK) that is used to
    -- encrypt your data. Amazon Kendra doesn\'t support asymmetric CMKs.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The current status of the index. When the value is @ACTIVE@, the index
    -- is ready for use. If the @Status@ field value is @FAILED@, the
    -- @ErrorMessage@ field contains a message that explains why.
    status :: Prelude.Maybe IndexStatus,
    -- | The Unix datetime that the index was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The user context policy for the Amazon Kendra index.
    userContextPolicy :: Prelude.Maybe UserContextPolicy,
    -- | Whether you have enabled the configuration for fetching access levels of
    -- groups and users from an IAM Identity Center (successor to Single
    -- Sign-On) identity source.
    userGroupResolutionConfiguration :: Prelude.Maybe UserGroupResolutionConfiguration,
    -- | The user token configuration for the Amazon Kendra index.
    userTokenConfigurations :: Prelude.Maybe [UserTokenConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityUnits', 'describeIndexResponse_capacityUnits' - For Enterprise Edition indexes, you can choose to use additional
-- capacity to meet the needs of your application. This contains the
-- capacity units used for the index. A query or document storage capacity
-- of zero indicates that the index is using the default capacity. For more
-- information on the default capacity for an index and adjusting this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
--
-- 'createdAt', 'describeIndexResponse_createdAt' - The Unix datetime that the index was created.
--
-- 'description', 'describeIndexResponse_description' - The description for the index.
--
-- 'documentMetadataConfigurations', 'describeIndexResponse_documentMetadataConfigurations' - Configuration information for document metadata or fields. Document
-- metadata are fields or attributes associated with your documents. For
-- example, the company department name associated with each document.
--
-- 'edition', 'describeIndexResponse_edition' - The Amazon Kendra edition used for the index. You decide the edition
-- when you create the index.
--
-- 'errorMessage', 'describeIndexResponse_errorMessage' - When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
--
-- 'id', 'describeIndexResponse_id' - The identifier of the index.
--
-- 'indexStatistics', 'describeIndexResponse_indexStatistics' - Provides information about the number of FAQ questions and answers and
-- the number of text documents indexed.
--
-- 'name', 'describeIndexResponse_name' - The name of the index.
--
-- 'roleArn', 'describeIndexResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
-- permission to write to your Amazon Cloudwatch logs.
--
-- 'serverSideEncryptionConfiguration', 'describeIndexResponse_serverSideEncryptionConfiguration' - The identifier of the KMScustomer master key (CMK) that is used to
-- encrypt your data. Amazon Kendra doesn\'t support asymmetric CMKs.
--
-- 'status', 'describeIndexResponse_status' - The current status of the index. When the value is @ACTIVE@, the index
-- is ready for use. If the @Status@ field value is @FAILED@, the
-- @ErrorMessage@ field contains a message that explains why.
--
-- 'updatedAt', 'describeIndexResponse_updatedAt' - The Unix datetime that the index was last updated.
--
-- 'userContextPolicy', 'describeIndexResponse_userContextPolicy' - The user context policy for the Amazon Kendra index.
--
-- 'userGroupResolutionConfiguration', 'describeIndexResponse_userGroupResolutionConfiguration' - Whether you have enabled the configuration for fetching access levels of
-- groups and users from an IAM Identity Center (successor to Single
-- Sign-On) identity source.
--
-- 'userTokenConfigurations', 'describeIndexResponse_userTokenConfigurations' - The user token configuration for the Amazon Kendra index.
--
-- 'httpStatus', 'describeIndexResponse_httpStatus' - The response's http status code.
newDescribeIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIndexResponse
newDescribeIndexResponse pHttpStatus_ =
  DescribeIndexResponse'
    { capacityUnits =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      documentMetadataConfigurations = Prelude.Nothing,
      edition = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      indexStatistics = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      userContextPolicy = Prelude.Nothing,
      userGroupResolutionConfiguration = Prelude.Nothing,
      userTokenConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | For Enterprise Edition indexes, you can choose to use additional
-- capacity to meet the needs of your application. This contains the
-- capacity units used for the index. A query or document storage capacity
-- of zero indicates that the index is using the default capacity. For more
-- information on the default capacity for an index and adjusting this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
describeIndexResponse_capacityUnits :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe CapacityUnitsConfiguration)
describeIndexResponse_capacityUnits = Lens.lens (\DescribeIndexResponse' {capacityUnits} -> capacityUnits) (\s@DescribeIndexResponse' {} a -> s {capacityUnits = a} :: DescribeIndexResponse)

-- | The Unix datetime that the index was created.
describeIndexResponse_createdAt :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.UTCTime)
describeIndexResponse_createdAt = Lens.lens (\DescribeIndexResponse' {createdAt} -> createdAt) (\s@DescribeIndexResponse' {} a -> s {createdAt = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Data._Time

-- | The description for the index.
describeIndexResponse_description :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_description = Lens.lens (\DescribeIndexResponse' {description} -> description) (\s@DescribeIndexResponse' {} a -> s {description = a} :: DescribeIndexResponse)

-- | Configuration information for document metadata or fields. Document
-- metadata are fields or attributes associated with your documents. For
-- example, the company department name associated with each document.
describeIndexResponse_documentMetadataConfigurations :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe [DocumentMetadataConfiguration])
describeIndexResponse_documentMetadataConfigurations = Lens.lens (\DescribeIndexResponse' {documentMetadataConfigurations} -> documentMetadataConfigurations) (\s@DescribeIndexResponse' {} a -> s {documentMetadataConfigurations = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Kendra edition used for the index. You decide the edition
-- when you create the index.
describeIndexResponse_edition :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexEdition)
describeIndexResponse_edition = Lens.lens (\DescribeIndexResponse' {edition} -> edition) (\s@DescribeIndexResponse' {} a -> s {edition = a} :: DescribeIndexResponse)

-- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
describeIndexResponse_errorMessage :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_errorMessage = Lens.lens (\DescribeIndexResponse' {errorMessage} -> errorMessage) (\s@DescribeIndexResponse' {} a -> s {errorMessage = a} :: DescribeIndexResponse)

-- | The identifier of the index.
describeIndexResponse_id :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_id = Lens.lens (\DescribeIndexResponse' {id} -> id) (\s@DescribeIndexResponse' {} a -> s {id = a} :: DescribeIndexResponse)

-- | Provides information about the number of FAQ questions and answers and
-- the number of text documents indexed.
describeIndexResponse_indexStatistics :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatistics)
describeIndexResponse_indexStatistics = Lens.lens (\DescribeIndexResponse' {indexStatistics} -> indexStatistics) (\s@DescribeIndexResponse' {} a -> s {indexStatistics = a} :: DescribeIndexResponse)

-- | The name of the index.
describeIndexResponse_name :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_name = Lens.lens (\DescribeIndexResponse' {name} -> name) (\s@DescribeIndexResponse' {} a -> s {name = a} :: DescribeIndexResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
-- permission to write to your Amazon Cloudwatch logs.
describeIndexResponse_roleArn :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_roleArn = Lens.lens (\DescribeIndexResponse' {roleArn} -> roleArn) (\s@DescribeIndexResponse' {} a -> s {roleArn = a} :: DescribeIndexResponse)

-- | The identifier of the KMScustomer master key (CMK) that is used to
-- encrypt your data. Amazon Kendra doesn\'t support asymmetric CMKs.
describeIndexResponse_serverSideEncryptionConfiguration :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe ServerSideEncryptionConfiguration)
describeIndexResponse_serverSideEncryptionConfiguration = Lens.lens (\DescribeIndexResponse' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@DescribeIndexResponse' {} a -> s {serverSideEncryptionConfiguration = a} :: DescribeIndexResponse)

-- | The current status of the index. When the value is @ACTIVE@, the index
-- is ready for use. If the @Status@ field value is @FAILED@, the
-- @ErrorMessage@ field contains a message that explains why.
describeIndexResponse_status :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatus)
describeIndexResponse_status = Lens.lens (\DescribeIndexResponse' {status} -> status) (\s@DescribeIndexResponse' {} a -> s {status = a} :: DescribeIndexResponse)

-- | The Unix datetime that the index was last updated.
describeIndexResponse_updatedAt :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.UTCTime)
describeIndexResponse_updatedAt = Lens.lens (\DescribeIndexResponse' {updatedAt} -> updatedAt) (\s@DescribeIndexResponse' {} a -> s {updatedAt = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Data._Time

-- | The user context policy for the Amazon Kendra index.
describeIndexResponse_userContextPolicy :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe UserContextPolicy)
describeIndexResponse_userContextPolicy = Lens.lens (\DescribeIndexResponse' {userContextPolicy} -> userContextPolicy) (\s@DescribeIndexResponse' {} a -> s {userContextPolicy = a} :: DescribeIndexResponse)

-- | Whether you have enabled the configuration for fetching access levels of
-- groups and users from an IAM Identity Center (successor to Single
-- Sign-On) identity source.
describeIndexResponse_userGroupResolutionConfiguration :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe UserGroupResolutionConfiguration)
describeIndexResponse_userGroupResolutionConfiguration = Lens.lens (\DescribeIndexResponse' {userGroupResolutionConfiguration} -> userGroupResolutionConfiguration) (\s@DescribeIndexResponse' {} a -> s {userGroupResolutionConfiguration = a} :: DescribeIndexResponse)

-- | The user token configuration for the Amazon Kendra index.
describeIndexResponse_userTokenConfigurations :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe [UserTokenConfiguration])
describeIndexResponse_userTokenConfigurations = Lens.lens (\DescribeIndexResponse' {userTokenConfigurations} -> userTokenConfigurations) (\s@DescribeIndexResponse' {} a -> s {userTokenConfigurations = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeIndexResponse_httpStatus :: Lens.Lens' DescribeIndexResponse Prelude.Int
describeIndexResponse_httpStatus = Lens.lens (\DescribeIndexResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexResponse' {} a -> s {httpStatus = a} :: DescribeIndexResponse)

instance Prelude.NFData DescribeIndexResponse where
  rnf DescribeIndexResponse' {..} =
    Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf documentMetadataConfigurations
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexStatistics
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf userContextPolicy
      `Prelude.seq` Prelude.rnf
        userGroupResolutionConfiguration
      `Prelude.seq` Prelude.rnf userTokenConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
