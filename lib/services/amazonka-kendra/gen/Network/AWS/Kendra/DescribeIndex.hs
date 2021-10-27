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
-- Module      : Network.AWS.Kendra.DescribeIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing Amazon Kendra index
module Network.AWS.Kendra.DescribeIndex
  ( -- * Creating a Request
    DescribeIndex (..),
    newDescribeIndex,

    -- * Request Lenses
    describeIndex_id,

    -- * Destructuring the Response
    DescribeIndexResponse (..),
    newDescribeIndexResponse,

    -- * Response Lenses
    describeIndexResponse_edition,
    describeIndexResponse_status,
    describeIndexResponse_createdAt,
    describeIndexResponse_documentMetadataConfigurations,
    describeIndexResponse_capacityUnits,
    describeIndexResponse_name,
    describeIndexResponse_id,
    describeIndexResponse_updatedAt,
    describeIndexResponse_userGroupResolutionConfiguration,
    describeIndexResponse_indexStatistics,
    describeIndexResponse_errorMessage,
    describeIndexResponse_description,
    describeIndexResponse_userContextPolicy,
    describeIndexResponse_userTokenConfigurations,
    describeIndexResponse_serverSideEncryptionConfiguration,
    describeIndexResponse_roleArn,
    describeIndexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIndex' smart constructor.
data DescribeIndex = DescribeIndex'
  { -- | The name of the index to describe.
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
-- 'id', 'describeIndex_id' - The name of the index to describe.
newDescribeIndex ::
  -- | 'id'
  Prelude.Text ->
  DescribeIndex
newDescribeIndex pId_ = DescribeIndex' {id = pId_}

-- | The name of the index to describe.
describeIndex_id :: Lens.Lens' DescribeIndex Prelude.Text
describeIndex_id = Lens.lens (\DescribeIndex' {id} -> id) (\s@DescribeIndex' {} a -> s {id = a} :: DescribeIndex)

instance Core.AWSRequest DescribeIndex where
  type
    AWSResponse DescribeIndex =
      DescribeIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            Prelude.<$> (x Core..?> "Edition")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> ( x Core..?> "DocumentMetadataConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "CapacityUnits")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "UserGroupResolutionConfiguration")
            Prelude.<*> (x Core..?> "IndexStatistics")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "UserContextPolicy")
            Prelude.<*> ( x Core..?> "UserTokenConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIndex

instance Prelude.NFData DescribeIndex

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
  { -- | The Amazon Kendra edition used for the index. You decide the edition
    -- when you create the index.
    edition :: Prelude.Maybe IndexEdition,
    -- | The current status of the index. When the value is @ACTIVE@, the index
    -- is ready for use. If the @Status@ field value is @FAILED@, the
    -- @ErrorMessage@ field contains a message that explains why.
    status :: Prelude.Maybe IndexStatus,
    -- | The Unix datetime that the index was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Configuration settings for any metadata applied to the documents in the
    -- index.
    documentMetadataConfigurations :: Prelude.Maybe [DocumentMetadataConfiguration],
    -- | For Enterprise edition indexes, you can choose to use additional
    -- capacity to meet the needs of your application. This contains the
    -- capacity units used for the index. A 0 for the query capacity or the
    -- storage capacity indicates that the index is using the default capacity
    -- for the index.
    capacityUnits :: Prelude.Maybe CapacityUnitsConfiguration,
    -- | The name of the index.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the index.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Unix datetime that the index was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | Shows whether you have enabled the configuration for fetching access
    -- levels of groups and users from an AWS Single Sign-On identity source.
    userGroupResolutionConfiguration :: Prelude.Maybe UserGroupResolutionConfiguration,
    -- | Provides information about the number of FAQ questions and answers and
    -- the number of text documents indexed.
    indexStatistics :: Prelude.Maybe IndexStatistics,
    -- | When th e@Status@ field value is @FAILED@, the @ErrorMessage@ field
    -- contains a message that explains why.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The description of the index.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user context policy for the Amazon Kendra index.
    userContextPolicy :: Prelude.Maybe UserContextPolicy,
    -- | The user token configuration for the Amazon Kendra index.
    userTokenConfigurations :: Prelude.Maybe [UserTokenConfiguration],
    -- | The identifier of the KMScustomer master key (CMK) used to encrypt your
    -- data. Amazon Kendra doesn\'t support asymmetric CMKs.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
    -- permission to write to your Amazon Cloudwatch logs.
    roleArn :: Prelude.Maybe Prelude.Text,
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
-- 'edition', 'describeIndexResponse_edition' - The Amazon Kendra edition used for the index. You decide the edition
-- when you create the index.
--
-- 'status', 'describeIndexResponse_status' - The current status of the index. When the value is @ACTIVE@, the index
-- is ready for use. If the @Status@ field value is @FAILED@, the
-- @ErrorMessage@ field contains a message that explains why.
--
-- 'createdAt', 'describeIndexResponse_createdAt' - The Unix datetime that the index was created.
--
-- 'documentMetadataConfigurations', 'describeIndexResponse_documentMetadataConfigurations' - Configuration settings for any metadata applied to the documents in the
-- index.
--
-- 'capacityUnits', 'describeIndexResponse_capacityUnits' - For Enterprise edition indexes, you can choose to use additional
-- capacity to meet the needs of your application. This contains the
-- capacity units used for the index. A 0 for the query capacity or the
-- storage capacity indicates that the index is using the default capacity
-- for the index.
--
-- 'name', 'describeIndexResponse_name' - The name of the index.
--
-- 'id', 'describeIndexResponse_id' - The name of the index.
--
-- 'updatedAt', 'describeIndexResponse_updatedAt' - The Unix datetime that the index was last updated.
--
-- 'userGroupResolutionConfiguration', 'describeIndexResponse_userGroupResolutionConfiguration' - Shows whether you have enabled the configuration for fetching access
-- levels of groups and users from an AWS Single Sign-On identity source.
--
-- 'indexStatistics', 'describeIndexResponse_indexStatistics' - Provides information about the number of FAQ questions and answers and
-- the number of text documents indexed.
--
-- 'errorMessage', 'describeIndexResponse_errorMessage' - When th e@Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
--
-- 'description', 'describeIndexResponse_description' - The description of the index.
--
-- 'userContextPolicy', 'describeIndexResponse_userContextPolicy' - The user context policy for the Amazon Kendra index.
--
-- 'userTokenConfigurations', 'describeIndexResponse_userTokenConfigurations' - The user token configuration for the Amazon Kendra index.
--
-- 'serverSideEncryptionConfiguration', 'describeIndexResponse_serverSideEncryptionConfiguration' - The identifier of the KMScustomer master key (CMK) used to encrypt your
-- data. Amazon Kendra doesn\'t support asymmetric CMKs.
--
-- 'roleArn', 'describeIndexResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
-- permission to write to your Amazon Cloudwatch logs.
--
-- 'httpStatus', 'describeIndexResponse_httpStatus' - The response's http status code.
newDescribeIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIndexResponse
newDescribeIndexResponse pHttpStatus_ =
  DescribeIndexResponse'
    { edition = Prelude.Nothing,
      status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      documentMetadataConfigurations = Prelude.Nothing,
      capacityUnits = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      userGroupResolutionConfiguration = Prelude.Nothing,
      indexStatistics = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      description = Prelude.Nothing,
      userContextPolicy = Prelude.Nothing,
      userTokenConfigurations = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Kendra edition used for the index. You decide the edition
-- when you create the index.
describeIndexResponse_edition :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexEdition)
describeIndexResponse_edition = Lens.lens (\DescribeIndexResponse' {edition} -> edition) (\s@DescribeIndexResponse' {} a -> s {edition = a} :: DescribeIndexResponse)

-- | The current status of the index. When the value is @ACTIVE@, the index
-- is ready for use. If the @Status@ field value is @FAILED@, the
-- @ErrorMessage@ field contains a message that explains why.
describeIndexResponse_status :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatus)
describeIndexResponse_status = Lens.lens (\DescribeIndexResponse' {status} -> status) (\s@DescribeIndexResponse' {} a -> s {status = a} :: DescribeIndexResponse)

-- | The Unix datetime that the index was created.
describeIndexResponse_createdAt :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.UTCTime)
describeIndexResponse_createdAt = Lens.lens (\DescribeIndexResponse' {createdAt} -> createdAt) (\s@DescribeIndexResponse' {} a -> s {createdAt = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Core._Time

-- | Configuration settings for any metadata applied to the documents in the
-- index.
describeIndexResponse_documentMetadataConfigurations :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe [DocumentMetadataConfiguration])
describeIndexResponse_documentMetadataConfigurations = Lens.lens (\DescribeIndexResponse' {documentMetadataConfigurations} -> documentMetadataConfigurations) (\s@DescribeIndexResponse' {} a -> s {documentMetadataConfigurations = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | For Enterprise edition indexes, you can choose to use additional
-- capacity to meet the needs of your application. This contains the
-- capacity units used for the index. A 0 for the query capacity or the
-- storage capacity indicates that the index is using the default capacity
-- for the index.
describeIndexResponse_capacityUnits :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe CapacityUnitsConfiguration)
describeIndexResponse_capacityUnits = Lens.lens (\DescribeIndexResponse' {capacityUnits} -> capacityUnits) (\s@DescribeIndexResponse' {} a -> s {capacityUnits = a} :: DescribeIndexResponse)

-- | The name of the index.
describeIndexResponse_name :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_name = Lens.lens (\DescribeIndexResponse' {name} -> name) (\s@DescribeIndexResponse' {} a -> s {name = a} :: DescribeIndexResponse)

-- | The name of the index.
describeIndexResponse_id :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_id = Lens.lens (\DescribeIndexResponse' {id} -> id) (\s@DescribeIndexResponse' {} a -> s {id = a} :: DescribeIndexResponse)

-- | The Unix datetime that the index was last updated.
describeIndexResponse_updatedAt :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.UTCTime)
describeIndexResponse_updatedAt = Lens.lens (\DescribeIndexResponse' {updatedAt} -> updatedAt) (\s@DescribeIndexResponse' {} a -> s {updatedAt = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Core._Time

-- | Shows whether you have enabled the configuration for fetching access
-- levels of groups and users from an AWS Single Sign-On identity source.
describeIndexResponse_userGroupResolutionConfiguration :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe UserGroupResolutionConfiguration)
describeIndexResponse_userGroupResolutionConfiguration = Lens.lens (\DescribeIndexResponse' {userGroupResolutionConfiguration} -> userGroupResolutionConfiguration) (\s@DescribeIndexResponse' {} a -> s {userGroupResolutionConfiguration = a} :: DescribeIndexResponse)

-- | Provides information about the number of FAQ questions and answers and
-- the number of text documents indexed.
describeIndexResponse_indexStatistics :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatistics)
describeIndexResponse_indexStatistics = Lens.lens (\DescribeIndexResponse' {indexStatistics} -> indexStatistics) (\s@DescribeIndexResponse' {} a -> s {indexStatistics = a} :: DescribeIndexResponse)

-- | When th e@Status@ field value is @FAILED@, the @ErrorMessage@ field
-- contains a message that explains why.
describeIndexResponse_errorMessage :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_errorMessage = Lens.lens (\DescribeIndexResponse' {errorMessage} -> errorMessage) (\s@DescribeIndexResponse' {} a -> s {errorMessage = a} :: DescribeIndexResponse)

-- | The description of the index.
describeIndexResponse_description :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_description = Lens.lens (\DescribeIndexResponse' {description} -> description) (\s@DescribeIndexResponse' {} a -> s {description = a} :: DescribeIndexResponse)

-- | The user context policy for the Amazon Kendra index.
describeIndexResponse_userContextPolicy :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe UserContextPolicy)
describeIndexResponse_userContextPolicy = Lens.lens (\DescribeIndexResponse' {userContextPolicy} -> userContextPolicy) (\s@DescribeIndexResponse' {} a -> s {userContextPolicy = a} :: DescribeIndexResponse)

-- | The user token configuration for the Amazon Kendra index.
describeIndexResponse_userTokenConfigurations :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe [UserTokenConfiguration])
describeIndexResponse_userTokenConfigurations = Lens.lens (\DescribeIndexResponse' {userTokenConfigurations} -> userTokenConfigurations) (\s@DescribeIndexResponse' {} a -> s {userTokenConfigurations = a} :: DescribeIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the KMScustomer master key (CMK) used to encrypt your
-- data. Amazon Kendra doesn\'t support asymmetric CMKs.
describeIndexResponse_serverSideEncryptionConfiguration :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe ServerSideEncryptionConfiguration)
describeIndexResponse_serverSideEncryptionConfiguration = Lens.lens (\DescribeIndexResponse' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@DescribeIndexResponse' {} a -> s {serverSideEncryptionConfiguration = a} :: DescribeIndexResponse)

-- | The Amazon Resource Name (ARN) of the IAM role that gives Amazon Kendra
-- permission to write to your Amazon Cloudwatch logs.
describeIndexResponse_roleArn :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_roleArn = Lens.lens (\DescribeIndexResponse' {roleArn} -> roleArn) (\s@DescribeIndexResponse' {} a -> s {roleArn = a} :: DescribeIndexResponse)

-- | The response's http status code.
describeIndexResponse_httpStatus :: Lens.Lens' DescribeIndexResponse Prelude.Int
describeIndexResponse_httpStatus = Lens.lens (\DescribeIndexResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexResponse' {} a -> s {httpStatus = a} :: DescribeIndexResponse)

instance Prelude.NFData DescribeIndexResponse
