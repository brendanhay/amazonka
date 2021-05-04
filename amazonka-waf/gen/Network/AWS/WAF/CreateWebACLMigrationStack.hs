{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAF.CreateWebACLMigrationStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation WAFV2 template for the specified web ACL
-- in the specified Amazon S3 bucket. Then, in CloudFormation, you create a
-- stack from the template, to create the web ACL and its resources in AWS
-- WAFV2. Use this to migrate your AWS WAF Classic web ACL to the latest
-- version of AWS WAF.
--
-- This is part of a larger migration procedure for web ACLs from AWS WAF
-- Classic to the latest version of AWS WAF. For the full procedure,
-- including caveats and manual steps to complete the migration and switch
-- over to the new web ACL, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-migrating-from-classic.html Migrating your AWS WAF Classic resources to AWS WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
module Network.AWS.WAF.CreateWebACLMigrationStack
  ( -- * Creating a Request
    CreateWebACLMigrationStack (..),
    newCreateWebACLMigrationStack,

    -- * Request Lenses
    createWebACLMigrationStack_webACLId,
    createWebACLMigrationStack_s3BucketName,
    createWebACLMigrationStack_ignoreUnsupportedType,

    -- * Destructuring the Response
    CreateWebACLMigrationStackResponse (..),
    newCreateWebACLMigrationStackResponse,

    -- * Response Lenses
    createWebACLMigrationStackResponse_httpStatus,
    createWebACLMigrationStackResponse_s3ObjectUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newCreateWebACLMigrationStack' smart constructor.
data CreateWebACLMigrationStack = CreateWebACLMigrationStack'
  { -- | The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
    webACLId :: Prelude.Text,
    -- | The name of the Amazon S3 bucket to store the CloudFormation template
    -- in. The S3 bucket must be configured as follows for the migration:
    --
    -- -   The bucket name must start with @aws-waf-migration-@. For example,
    --     @aws-waf-migration-my-web-acl@.
    --
    -- -   The bucket must be in the Region where you are deploying the
    --     template. For example, for a web ACL in us-west-2, you must use an
    --     Amazon S3 bucket in us-west-2 and you must deploy the template stack
    --     to us-west-2.
    --
    -- -   The bucket policies must permit the migration process to write data.
    --     For listings of the bucket policies, see the Examples section.
    s3BucketName :: Prelude.Text,
    -- | Indicates whether to exclude entities that can\'t be migrated or to stop
    -- the migration. Set this to true to ignore unsupported entities in the
    -- web ACL during the migration. Otherwise, if AWS WAF encounters
    -- unsupported entities, it stops the process and throws an exception.
    ignoreUnsupportedType :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateWebACLMigrationStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLId', 'createWebACLMigrationStack_webACLId' - The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
--
-- 's3BucketName', 'createWebACLMigrationStack_s3BucketName' - The name of the Amazon S3 bucket to store the CloudFormation template
-- in. The S3 bucket must be configured as follows for the migration:
--
-- -   The bucket name must start with @aws-waf-migration-@. For example,
--     @aws-waf-migration-my-web-acl@.
--
-- -   The bucket must be in the Region where you are deploying the
--     template. For example, for a web ACL in us-west-2, you must use an
--     Amazon S3 bucket in us-west-2 and you must deploy the template stack
--     to us-west-2.
--
-- -   The bucket policies must permit the migration process to write data.
--     For listings of the bucket policies, see the Examples section.
--
-- 'ignoreUnsupportedType', 'createWebACLMigrationStack_ignoreUnsupportedType' - Indicates whether to exclude entities that can\'t be migrated or to stop
-- the migration. Set this to true to ignore unsupported entities in the
-- web ACL during the migration. Otherwise, if AWS WAF encounters
-- unsupported entities, it stops the process and throws an exception.
newCreateWebACLMigrationStack ::
  -- | 'webACLId'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 'ignoreUnsupportedType'
  Prelude.Bool ->
  CreateWebACLMigrationStack
newCreateWebACLMigrationStack
  pWebACLId_
  pS3BucketName_
  pIgnoreUnsupportedType_ =
    CreateWebACLMigrationStack'
      { webACLId = pWebACLId_,
        s3BucketName = pS3BucketName_,
        ignoreUnsupportedType = pIgnoreUnsupportedType_
      }

-- | The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
createWebACLMigrationStack_webACLId :: Lens.Lens' CreateWebACLMigrationStack Prelude.Text
createWebACLMigrationStack_webACLId = Lens.lens (\CreateWebACLMigrationStack' {webACLId} -> webACLId) (\s@CreateWebACLMigrationStack' {} a -> s {webACLId = a} :: CreateWebACLMigrationStack)

-- | The name of the Amazon S3 bucket to store the CloudFormation template
-- in. The S3 bucket must be configured as follows for the migration:
--
-- -   The bucket name must start with @aws-waf-migration-@. For example,
--     @aws-waf-migration-my-web-acl@.
--
-- -   The bucket must be in the Region where you are deploying the
--     template. For example, for a web ACL in us-west-2, you must use an
--     Amazon S3 bucket in us-west-2 and you must deploy the template stack
--     to us-west-2.
--
-- -   The bucket policies must permit the migration process to write data.
--     For listings of the bucket policies, see the Examples section.
createWebACLMigrationStack_s3BucketName :: Lens.Lens' CreateWebACLMigrationStack Prelude.Text
createWebACLMigrationStack_s3BucketName = Lens.lens (\CreateWebACLMigrationStack' {s3BucketName} -> s3BucketName) (\s@CreateWebACLMigrationStack' {} a -> s {s3BucketName = a} :: CreateWebACLMigrationStack)

-- | Indicates whether to exclude entities that can\'t be migrated or to stop
-- the migration. Set this to true to ignore unsupported entities in the
-- web ACL during the migration. Otherwise, if AWS WAF encounters
-- unsupported entities, it stops the process and throws an exception.
createWebACLMigrationStack_ignoreUnsupportedType :: Lens.Lens' CreateWebACLMigrationStack Prelude.Bool
createWebACLMigrationStack_ignoreUnsupportedType = Lens.lens (\CreateWebACLMigrationStack' {ignoreUnsupportedType} -> ignoreUnsupportedType) (\s@CreateWebACLMigrationStack' {} a -> s {ignoreUnsupportedType = a} :: CreateWebACLMigrationStack)

instance
  Prelude.AWSRequest
    CreateWebACLMigrationStack
  where
  type
    Rs CreateWebACLMigrationStack =
      CreateWebACLMigrationStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebACLMigrationStackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "S3ObjectUrl")
      )

instance Prelude.Hashable CreateWebACLMigrationStack

instance Prelude.NFData CreateWebACLMigrationStack

instance Prelude.ToHeaders CreateWebACLMigrationStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.CreateWebACLMigrationStack" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateWebACLMigrationStack where
  toJSON CreateWebACLMigrationStack' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WebACLId" Prelude..= webACLId),
            Prelude.Just
              ("S3BucketName" Prelude..= s3BucketName),
            Prelude.Just
              ( "IgnoreUnsupportedType"
                  Prelude..= ignoreUnsupportedType
              )
          ]
      )

instance Prelude.ToPath CreateWebACLMigrationStack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateWebACLMigrationStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWebACLMigrationStackResponse' smart constructor.
data CreateWebACLMigrationStackResponse = CreateWebACLMigrationStackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The URL of the template created in Amazon S3.
    s3ObjectUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateWebACLMigrationStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWebACLMigrationStackResponse_httpStatus' - The response's http status code.
--
-- 's3ObjectUrl', 'createWebACLMigrationStackResponse_s3ObjectUrl' - The URL of the template created in Amazon S3.
newCreateWebACLMigrationStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 's3ObjectUrl'
  Prelude.Text ->
  CreateWebACLMigrationStackResponse
newCreateWebACLMigrationStackResponse
  pHttpStatus_
  pS3ObjectUrl_ =
    CreateWebACLMigrationStackResponse'
      { httpStatus =
          pHttpStatus_,
        s3ObjectUrl = pS3ObjectUrl_
      }

-- | The response's http status code.
createWebACLMigrationStackResponse_httpStatus :: Lens.Lens' CreateWebACLMigrationStackResponse Prelude.Int
createWebACLMigrationStackResponse_httpStatus = Lens.lens (\CreateWebACLMigrationStackResponse' {httpStatus} -> httpStatus) (\s@CreateWebACLMigrationStackResponse' {} a -> s {httpStatus = a} :: CreateWebACLMigrationStackResponse)

-- | The URL of the template created in Amazon S3.
createWebACLMigrationStackResponse_s3ObjectUrl :: Lens.Lens' CreateWebACLMigrationStackResponse Prelude.Text
createWebACLMigrationStackResponse_s3ObjectUrl = Lens.lens (\CreateWebACLMigrationStackResponse' {s3ObjectUrl} -> s3ObjectUrl) (\s@CreateWebACLMigrationStackResponse' {} a -> s {s3ObjectUrl = a} :: CreateWebACLMigrationStackResponse)

instance
  Prelude.NFData
    CreateWebACLMigrationStackResponse
