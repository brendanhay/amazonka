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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    describeIndexResponse_name,
    describeIndexResponse_roleArn,
    describeIndexResponse_capacityUnits,
    describeIndexResponse_errorMessage,
    describeIndexResponse_userGroupResolutionConfiguration,
    describeIndexResponse_serverSideEncryptionConfiguration,
    describeIndexResponse_edition,
    describeIndexResponse_status,
    describeIndexResponse_id,
    describeIndexResponse_description,
    describeIndexResponse_userTokenConfigurations,
    describeIndexResponse_userContextPolicy,
    describeIndexResponse_indexStatistics,
    describeIndexResponse_createdAt,
    describeIndexResponse_updatedAt,
    describeIndexResponse_documentMetadataConfigurations,
    describeIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (x Core..?> "CapacityUnits")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "UserGroupResolutionConfiguration")
            Prelude.<*> (x Core..?> "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Core..?> "Edition")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> ( x Core..?> "UserTokenConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "UserContextPolicy")
            Prelude.<*> (x Core..?> "IndexStatistics")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> ( x Core..?> "DocumentMetadataConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIndex where
  hashWithSalt _salt DescribeIndex' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeIndex where
  rnf DescribeIndex' {..} = Prelude.rnf id

instance Core.ToHeaders DescribeIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeIndex where
  toJSON DescribeIndex' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("Id" Core..= id)])

instance Core.ToPath DescribeIndex where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { -- | The name of the index.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
    -- permission to write to your Amazon Cloudwatch logs.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | For Enterprise Edition indexes, you can choose to use additional
    -- capacity to meet the needs of your application. This contains the
    -- capacity units used for the index. A query or document storage capacity
    -- of zero indicates that the index is using the default capacity. For more
    -- information on the default capacity for an index and adjusting this, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- contains a message that explains why.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Whether you have enabled the configuration for fetching access levels of
    -- groups and users from an IAM Identity Center (successor to Single
    -- Sign-On) identity source.
    userGroupResolutionConfiguration :: Prelude.Maybe UserGroupResolutionConfiguration,
    -- | The identifier of the KMScustomer master key (CMK) that is used to
    -- encrypt your data. Amazon Kendra doesn\'t support asymmetric CMKs.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The Amazon Kendra edition used for the index. You decide the edition
    -- when you create the index.
    edition :: Prelude.Maybe IndexEdition,
    -- | The current status of the index. When the value is @ACTIVE@, the index
    -- is ready for use. If the @Status@ field value is @FAILED@, the
    -- @ErrorMessage@ field contains a message that explains why.
    status :: Prelude.Maybe IndexStatus,
    -- | The identifier of the index.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description for the index.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user token configuration for the Amazon Kendra index.
    userTokenConfigurations :: Prelude.Maybe [UserTokenConfiguration],
    -- | The user context policy for the Amazon Kendra index.
    userContextPolicy :: Prelude.Maybe UserContextPolicy,
    -- | Provides information about the number of FAQ questions and answers and
    -- the number of text documents indexed.
    indexStatistics :: Prelude.Maybe IndexStatistics,
    -- | The Unix datetime that the index was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix datetime that the index was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | Configuration information for document metadata or fields. Document
    -- metadata are fields or attributes associated with your documents. For
    -- example, the company department name associated with each document.
    documentMetadataConfigurations :: Prelude.Maybe [DocumentMetadataConfiguration],
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
-- 'name', 'describeIndexResponse_name' - The name of the index.
--
-- 'roleArn', 'describeIndexResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
-- permission to write to your Amazon Cloudwatch logs.
--
-- 'capacityUnits', 'describeIndexResponse_capacityUnits' - For Enterprise Edition indexes, you can choose to use additional
-- capacity to meet the needs of your application. This contains the
-- capacity units used for the index. A query or document storage capacity
-- of zero indicates that the index is using the default capacity. For more
-- information on the default capacity for an index and adjusting this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
--
-- 'errorMessage', 'describeIndexResponse_errorMessage' - When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
--
-- 'userGroupResolutionConfiguration', 'describeIndexResponse_userGroupResolutionConfiguration' - Whether you have enabled the configuration for fetching access levels of
-- groups and users from an IAM Identity Center (successor to Single
-- Sign-On) identity source.
--
-- 'serverSideEncryptionConfiguration', 'describeIndexResponse_serverSideEncryptionConfiguration' - The identifier of the KMScustomer master key (CMK) that is used to
-- encrypt your data. Amazon Kendra doesn\'t support asymmetric CMKs.
--
-- 'edition', 'describeIndexResponse_edition' - The Amazon Kendra edition used for the index. You decide the edition
-- when you create the index.
--
-- 'status', 'describeIndexResponse_status' - The current status of the index. When the value is @ACTIVE@, the index
-- is ready for use. If the @Status@ field value is @FAILED@, the
-- @ErrorMessage@ field contains a message that explains why.
--
-- 'id', 'describeIndexResponse_id' - The identifier of the index.
--
-- 'description', 'describeIndexResponse_description' - The description for the index.
--
-- 'userTokenConfigurations', 'describeIndexResponse_userTokenConfigurations' - The user token configuration for the Amazon Kendra index.
--
-- 'userContextPolicy', 'describeIndexResponse_userContextPolicy' - The user context policy for the Amazon Kendra index.
--
-- 'indexStatistics', 'describeIndexResponse_indexStatistics' - Provides information about the number of FAQ questions and answers and
-- the number of text documents indexed.
--
-- 'createdAt', 'describeIndexResponse_createdAt' - The Unix datetime that the index was created.
--
-- 'updatedAt', 'describeIndexResponse_updatedAt' - The Unix datetime that the index was last updated.
--
-- 'documentMetadataConfigurations', 'describeIndexResponse_documentMetadataConfigurations' - Configuration information for document metadata or fields. Document
-- metadata are fields or attributes associated with your documents. For
-- example, the company department name associated with each document.
--
-- 'httpStatus', 'describeIndexResponse_httpStatus' - The response's http status code.
newDescribeIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIndexResponse
newDescribeIndexResponse pHttpStatus_ =
  DescribeIndexResponse'
    { name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      userGroupResolutionConfiguration = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      edition = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      userTokenConfigurations = Prelude.Nothing,
      userContextPolicy = Prelude.Nothing,
      indexStatistics = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      documentMetadataConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the index.
describeIndexResponse_name :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_name = Lens.lens (\DescribeIndexResponse' {name} -> name) (\s@DescribeIndexResponse' {} a -> s {name = a} :: DescribeIndexResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
-- permission to write to your Amazon Cloudwatch logs.
describeIndexResponse_roleArn :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_roleArn = Lens.lens (\DescribeIndexResponse' {roleArn} -> roleArn) (\s@DescribeIndexResponse' {} a -> s {roleArn = a} :: DescribeIndexResponse)

-- | For Enterprise Edition indexes, you can choose to use additional
-- capacity to meet the needs of your application. This contains the
-- capacity units used for the index. A query or document storage capacity
-- of zero indicates that the index is using the default capacity. For more
-- information on the default capacity for an index and adjusting this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/adjusting-capacity.html Adjusting capacity>.
describeIndexResponse_capacityUnits :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe CapacityUnitsConfiguration)
describeIndexResponse_capacityUnits = Lens.lens (\DescribeIndexResponse' {capacityUnits} -> capacityUnits) (\s@DescribeIndexResponse' {} a -> s {capacityUnits = a} :: DescribeIndexResponse)

-- | When the @Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
describeIndexResponse_errorMessage :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_errorMessage = Lens.lens (\DescribeIndexResponse' {errorMessage} -> errorMessage) (\s@DescribeIndexResponse' {} a -> s {errorMessage = a} :: DescribeIndexResponse)

-- | Whether you have enabled the configuration for fetching access levels of
-- groups and users from an IAM Identity Center (successor to Single
-- Sign-On) identity source.
describeIndexResponse_userGroupResolutionConfiguration :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe UserGroupResolutionConfiguration)
describeIndexResponse_userGroupResolutionConfiguration = Lens.lens (\DescribeIndexResponse' {userGroupResolutionConfiguration} -> userGroupResolutionConfiguration) (\s@DescribeIndexResponse' {} a -> s {userGroupResolutionConfiguration = a} :: DescribeIndexResponse)

-- | The identifier of the KMScustomer master key (CMK) that is used to
-- encrypt your data. Amazon Kendra doesn\'t support asymmetric CMKs.
describeIndexResponse_serverSideEncryptionConfiguration :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe ServerSideEncryptionConfiguration)
describeIndexResponse_serverSideEncryptionConfiguration = Lens.lens (\DescribeIndexResponse' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@DescribeIndexResponse' {} a -> s {serverSideEncryptionConfiguration = a} :: DescribeIndexResponse)

-- | The Amazon Kendra edition used for the index. You decide the edition
-- when you create the index.
describeIndexResponse_edition :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexEdition)
describeIndexResponse_edition = Lens.lens (\DescribeIndexResponse' {edition} -> edition) (\s@DescribeIndexResponse' {} a -> s {edition = a} :: DescribeIndexResponse)

-- | The current status of the index. When the value is @ACTIVE@, the index
-- is ready for use. If the @Status@ field value is @FAILED@, the
-- @ErrorMessage@ field contains a message that explains why.
describeIndexResponse_status :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatus)
describeIndexResponse_status = Lens.lens (\DescribeIndexResponse' {status} -> status) (\s@DescribeIndexResponse' {} a -> s {status = a} :: DescribeIndexResponse)

-- | The identifier of the index.
describeIndexResponse_id :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_id = Lens.lens (\DescribeIndexResponse' {id} -> id) (\s@DescribeIndexResponse' {} a -> s {id = a} :: DescribeIndexResponse)

-- | The description for the index.
describeIndexResponse_description :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_description = Lens.lens (\DescribeIndexResponse' {description} -> description) (\s@DescribeIndexResponse' {} a -> s {description = a} :: DescribeIndexResponse)

-- | The user token configuration for the Amazon Kendra index.
describeIndexResponse_userTokenConfigurations :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe [UserTokenConfiguration])
describeIndexResponse_userTokenConfigurations = Lens.lens (\DescribeIndexResponse' {userTokenConfigurations} -> userTokenConfigurations) (\s@DescribeIndexResponse' {} a -> s {userTokenConfigurations = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The user context policy for the Amazon Kendra index.
describeIndexResponse_userContextPolicy :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe UserContextPolicy)
describeIndexResponse_userContextPolicy = Lens.lens (\DescribeIndexResponse' {userContextPolicy} -> userContextPolicy) (\s@DescribeIndexResponse' {} a -> s {userContextPolicy = a} :: DescribeIndexResponse)

-- | Provides information about the number of FAQ questions and answers and
-- the number of text documents indexed.
describeIndexResponse_indexStatistics :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatistics)
describeIndexResponse_indexStatistics = Lens.lens (\DescribeIndexResponse' {indexStatistics} -> indexStatistics) (\s@DescribeIndexResponse' {} a -> s {indexStatistics = a} :: DescribeIndexResponse)

-- | The Unix datetime that the index was created.
describeIndexResponse_createdAt :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.UTCTime)
describeIndexResponse_createdAt = Lens.lens (\DescribeIndexResponse' {createdAt} -> createdAt) (\s@DescribeIndexResponse' {} a -> s {createdAt = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Core._Time

-- | The Unix datetime that the index was last updated.
describeIndexResponse_updatedAt :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.UTCTime)
describeIndexResponse_updatedAt = Lens.lens (\DescribeIndexResponse' {updatedAt} -> updatedAt) (\s@DescribeIndexResponse' {} a -> s {updatedAt = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Core._Time

-- | Configuration information for document metadata or fields. Document
-- metadata are fields or attributes associated with your documents. For
-- example, the company department name associated with each document.
describeIndexResponse_documentMetadataConfigurations :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe [DocumentMetadataConfiguration])
describeIndexResponse_documentMetadataConfigurations = Lens.lens (\DescribeIndexResponse' {documentMetadataConfigurations} -> documentMetadataConfigurations) (\s@DescribeIndexResponse' {} a -> s {documentMetadataConfigurations = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeIndexResponse_httpStatus :: Lens.Lens' DescribeIndexResponse Prelude.Int
describeIndexResponse_httpStatus = Lens.lens (\DescribeIndexResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexResponse' {} a -> s {httpStatus = a} :: DescribeIndexResponse)

instance Prelude.NFData DescribeIndexResponse where
  rnf DescribeIndexResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf capacityUnits
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf userGroupResolutionConfiguration
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf userTokenConfigurations
      `Prelude.seq` Prelude.rnf userContextPolicy
      `Prelude.seq` Prelude.rnf indexStatistics
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf
        documentMetadataConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
