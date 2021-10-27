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
-- Module      : Network.AWS.Kendra.CreateIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Kendra index. Index creation is an asynchronous
-- operation. To determine if index creation has completed, check the
-- @Status@ field returned from a call to @DescribeIndex@. The @Status@
-- field is set to @ACTIVE@ when the index is ready to use.
--
-- Once the index is active you can index your documents using the
-- @BatchPutDocument@ operation or using one of the supported data sources.
module Network.AWS.Kendra.CreateIndex
  ( -- * Creating a Request
    CreateIndex (..),
    newCreateIndex,

    -- * Request Lenses
    createIndex_edition,
    createIndex_clientToken,
    createIndex_userGroupResolutionConfiguration,
    createIndex_description,
    createIndex_userContextPolicy,
    createIndex_tags,
    createIndex_userTokenConfigurations,
    createIndex_serverSideEncryptionConfiguration,
    createIndex_name,
    createIndex_roleArn,

    -- * Destructuring the Response
    CreateIndexResponse (..),
    newCreateIndexResponse,

    -- * Response Lenses
    createIndexResponse_id,
    createIndexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIndex' smart constructor.
data CreateIndex = CreateIndex'
  { -- | The Amazon Kendra edition to use for the index. Choose
    -- @DEVELOPER_EDITION@ for indexes intended for development, testing, or
    -- proof of concept. Use @ENTERPRISE_EDITION@ for your production
    -- databases. Once you set the edition for an index, it can\'t be changed.
    --
    -- The @Edition@ parameter is optional. If you don\'t supply a value, the
    -- default is @ENTERPRISE_EDITION@.
    --
    -- For more information on quota limits for enterprise and developer
    -- editions, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas>.
    edition :: Prelude.Maybe IndexEdition,
    -- | A token that you provide to identify the request to create an index.
    -- Multiple calls to the @CreateIndex@ operation with the same client token
    -- will create only one index.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Enables fetching access levels of groups and users from an AWS Single
    -- Sign-On identity source. To configure this, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
    userGroupResolutionConfiguration :: Prelude.Maybe UserGroupResolutionConfiguration,
    -- | A description for the index.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user context policy.
    --
    -- [ATTRIBUTE_FILTER]
    --     All indexed content is searchable and displayable for all users. If
    --     you want to filter search results on user context, you can use the
    --     attribute filters of @_user_id@ and @_group_ids@ or you can provide
    --     user and group information in @UserContext@.
    --
    -- [USER_TOKEN]
    --     Enables token-based user access control to filter search results on
    --     user context. All documents with no access control and all documents
    --     accessible to the user will be searchable and displayable.
    userContextPolicy :: Prelude.Maybe UserContextPolicy,
    -- | A list of key-value pairs that identify the index. You can use the tags
    -- to identify and organize your resources and to control access to
    -- resources.
    tags :: Prelude.Maybe [Tag],
    -- | The user token configuration.
    userTokenConfigurations :: Prelude.Maybe [UserTokenConfiguration],
    -- | The identifier of the KMScustomer managed key (CMK) to use to encrypt
    -- data indexed by Amazon Kendra. Amazon Kendra doesn\'t support asymmetric
    -- CMKs.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The name for the new index.
    name :: Prelude.Text,
    -- | An Identity and Access Management(IAM) role that gives Amazon Kendra
    -- permissions to access your Amazon CloudWatch logs and metrics. This is
    -- also the role used when you use the @BatchPutDocument@ operation to
    -- index documents from an Amazon S3 bucket.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edition', 'createIndex_edition' - The Amazon Kendra edition to use for the index. Choose
-- @DEVELOPER_EDITION@ for indexes intended for development, testing, or
-- proof of concept. Use @ENTERPRISE_EDITION@ for your production
-- databases. Once you set the edition for an index, it can\'t be changed.
--
-- The @Edition@ parameter is optional. If you don\'t supply a value, the
-- default is @ENTERPRISE_EDITION@.
--
-- For more information on quota limits for enterprise and developer
-- editions, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas>.
--
-- 'clientToken', 'createIndex_clientToken' - A token that you provide to identify the request to create an index.
-- Multiple calls to the @CreateIndex@ operation with the same client token
-- will create only one index.
--
-- 'userGroupResolutionConfiguration', 'createIndex_userGroupResolutionConfiguration' - Enables fetching access levels of groups and users from an AWS Single
-- Sign-On identity source. To configure this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
--
-- 'description', 'createIndex_description' - A description for the index.
--
-- 'userContextPolicy', 'createIndex_userContextPolicy' - The user context policy.
--
-- [ATTRIBUTE_FILTER]
--     All indexed content is searchable and displayable for all users. If
--     you want to filter search results on user context, you can use the
--     attribute filters of @_user_id@ and @_group_ids@ or you can provide
--     user and group information in @UserContext@.
--
-- [USER_TOKEN]
--     Enables token-based user access control to filter search results on
--     user context. All documents with no access control and all documents
--     accessible to the user will be searchable and displayable.
--
-- 'tags', 'createIndex_tags' - A list of key-value pairs that identify the index. You can use the tags
-- to identify and organize your resources and to control access to
-- resources.
--
-- 'userTokenConfigurations', 'createIndex_userTokenConfigurations' - The user token configuration.
--
-- 'serverSideEncryptionConfiguration', 'createIndex_serverSideEncryptionConfiguration' - The identifier of the KMScustomer managed key (CMK) to use to encrypt
-- data indexed by Amazon Kendra. Amazon Kendra doesn\'t support asymmetric
-- CMKs.
--
-- 'name', 'createIndex_name' - The name for the new index.
--
-- 'roleArn', 'createIndex_roleArn' - An Identity and Access Management(IAM) role that gives Amazon Kendra
-- permissions to access your Amazon CloudWatch logs and metrics. This is
-- also the role used when you use the @BatchPutDocument@ operation to
-- index documents from an Amazon S3 bucket.
newCreateIndex ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateIndex
newCreateIndex pName_ pRoleArn_ =
  CreateIndex'
    { edition = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      userGroupResolutionConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      userContextPolicy = Prelude.Nothing,
      tags = Prelude.Nothing,
      userTokenConfigurations = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_
    }

-- | The Amazon Kendra edition to use for the index. Choose
-- @DEVELOPER_EDITION@ for indexes intended for development, testing, or
-- proof of concept. Use @ENTERPRISE_EDITION@ for your production
-- databases. Once you set the edition for an index, it can\'t be changed.
--
-- The @Edition@ parameter is optional. If you don\'t supply a value, the
-- default is @ENTERPRISE_EDITION@.
--
-- For more information on quota limits for enterprise and developer
-- editions, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas>.
createIndex_edition :: Lens.Lens' CreateIndex (Prelude.Maybe IndexEdition)
createIndex_edition = Lens.lens (\CreateIndex' {edition} -> edition) (\s@CreateIndex' {} a -> s {edition = a} :: CreateIndex)

-- | A token that you provide to identify the request to create an index.
-- Multiple calls to the @CreateIndex@ operation with the same client token
-- will create only one index.
createIndex_clientToken :: Lens.Lens' CreateIndex (Prelude.Maybe Prelude.Text)
createIndex_clientToken = Lens.lens (\CreateIndex' {clientToken} -> clientToken) (\s@CreateIndex' {} a -> s {clientToken = a} :: CreateIndex)

-- | Enables fetching access levels of groups and users from an AWS Single
-- Sign-On identity source. To configure this, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UserGroupResolutionConfiguration.html UserGroupResolutionConfiguration>.
createIndex_userGroupResolutionConfiguration :: Lens.Lens' CreateIndex (Prelude.Maybe UserGroupResolutionConfiguration)
createIndex_userGroupResolutionConfiguration = Lens.lens (\CreateIndex' {userGroupResolutionConfiguration} -> userGroupResolutionConfiguration) (\s@CreateIndex' {} a -> s {userGroupResolutionConfiguration = a} :: CreateIndex)

-- | A description for the index.
createIndex_description :: Lens.Lens' CreateIndex (Prelude.Maybe Prelude.Text)
createIndex_description = Lens.lens (\CreateIndex' {description} -> description) (\s@CreateIndex' {} a -> s {description = a} :: CreateIndex)

-- | The user context policy.
--
-- [ATTRIBUTE_FILTER]
--     All indexed content is searchable and displayable for all users. If
--     you want to filter search results on user context, you can use the
--     attribute filters of @_user_id@ and @_group_ids@ or you can provide
--     user and group information in @UserContext@.
--
-- [USER_TOKEN]
--     Enables token-based user access control to filter search results on
--     user context. All documents with no access control and all documents
--     accessible to the user will be searchable and displayable.
createIndex_userContextPolicy :: Lens.Lens' CreateIndex (Prelude.Maybe UserContextPolicy)
createIndex_userContextPolicy = Lens.lens (\CreateIndex' {userContextPolicy} -> userContextPolicy) (\s@CreateIndex' {} a -> s {userContextPolicy = a} :: CreateIndex)

-- | A list of key-value pairs that identify the index. You can use the tags
-- to identify and organize your resources and to control access to
-- resources.
createIndex_tags :: Lens.Lens' CreateIndex (Prelude.Maybe [Tag])
createIndex_tags = Lens.lens (\CreateIndex' {tags} -> tags) (\s@CreateIndex' {} a -> s {tags = a} :: CreateIndex) Prelude.. Lens.mapping Lens.coerced

-- | The user token configuration.
createIndex_userTokenConfigurations :: Lens.Lens' CreateIndex (Prelude.Maybe [UserTokenConfiguration])
createIndex_userTokenConfigurations = Lens.lens (\CreateIndex' {userTokenConfigurations} -> userTokenConfigurations) (\s@CreateIndex' {} a -> s {userTokenConfigurations = a} :: CreateIndex) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the KMScustomer managed key (CMK) to use to encrypt
-- data indexed by Amazon Kendra. Amazon Kendra doesn\'t support asymmetric
-- CMKs.
createIndex_serverSideEncryptionConfiguration :: Lens.Lens' CreateIndex (Prelude.Maybe ServerSideEncryptionConfiguration)
createIndex_serverSideEncryptionConfiguration = Lens.lens (\CreateIndex' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@CreateIndex' {} a -> s {serverSideEncryptionConfiguration = a} :: CreateIndex)

-- | The name for the new index.
createIndex_name :: Lens.Lens' CreateIndex Prelude.Text
createIndex_name = Lens.lens (\CreateIndex' {name} -> name) (\s@CreateIndex' {} a -> s {name = a} :: CreateIndex)

-- | An Identity and Access Management(IAM) role that gives Amazon Kendra
-- permissions to access your Amazon CloudWatch logs and metrics. This is
-- also the role used when you use the @BatchPutDocument@ operation to
-- index documents from an Amazon S3 bucket.
createIndex_roleArn :: Lens.Lens' CreateIndex Prelude.Text
createIndex_roleArn = Lens.lens (\CreateIndex' {roleArn} -> roleArn) (\s@CreateIndex' {} a -> s {roleArn = a} :: CreateIndex)

instance Core.AWSRequest CreateIndex where
  type AWSResponse CreateIndex = CreateIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIndexResponse'
            Prelude.<$> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIndex

instance Prelude.NFData CreateIndex

instance Core.ToHeaders CreateIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.CreateIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIndex where
  toJSON CreateIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Edition" Core..=) Prelude.<$> edition,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("UserGroupResolutionConfiguration" Core..=)
              Prelude.<$> userGroupResolutionConfiguration,
            ("Description" Core..=) Prelude.<$> description,
            ("UserContextPolicy" Core..=)
              Prelude.<$> userContextPolicy,
            ("Tags" Core..=) Prelude.<$> tags,
            ("UserTokenConfigurations" Core..=)
              Prelude.<$> userTokenConfigurations,
            ("ServerSideEncryptionConfiguration" Core..=)
              Prelude.<$> serverSideEncryptionConfiguration,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateIndex where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIndexResponse' smart constructor.
data CreateIndexResponse = CreateIndexResponse'
  { -- | The unique identifier of the index. Use this identifier when you query
    -- an index, set up a data source, or index a document.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createIndexResponse_id' - The unique identifier of the index. Use this identifier when you query
-- an index, set up a data source, or index a document.
--
-- 'httpStatus', 'createIndexResponse_httpStatus' - The response's http status code.
newCreateIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIndexResponse
newCreateIndexResponse pHttpStatus_ =
  CreateIndexResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the index. Use this identifier when you query
-- an index, set up a data source, or index a document.
createIndexResponse_id :: Lens.Lens' CreateIndexResponse (Prelude.Maybe Prelude.Text)
createIndexResponse_id = Lens.lens (\CreateIndexResponse' {id} -> id) (\s@CreateIndexResponse' {} a -> s {id = a} :: CreateIndexResponse)

-- | The response's http status code.
createIndexResponse_httpStatus :: Lens.Lens' CreateIndexResponse Prelude.Int
createIndexResponse_httpStatus = Lens.lens (\CreateIndexResponse' {httpStatus} -> httpStatus) (\s@CreateIndexResponse' {} a -> s {httpStatus = a} :: CreateIndexResponse)

instance Prelude.NFData CreateIndexResponse
