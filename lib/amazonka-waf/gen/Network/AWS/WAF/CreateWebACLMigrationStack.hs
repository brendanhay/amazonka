{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateWebACLMigrationStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation WAFV2 template for the specified web ACL in the specified Amazon S3 bucket. Then, in CloudFormation, you create a stack from the template, to create the web ACL and its resources in AWS WAFV2. Use this to migrate your AWS WAF Classic web ACL to the latest version of AWS WAF.
--
--
-- This is part of a larger migration procedure for web ACLs from AWS WAF Classic to the latest version of AWS WAF. For the full procedure, including caveats and manual steps to complete the migration and switch over to the new web ACL, see <https://docs.aws.amazon.com/waf/latest/developerguide/waf-migrating-from-classic.html Migrating your AWS WAF Classic resources to AWS WAF> in the <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateWebACLMigrationStack
  ( -- * Creating a Request
    createWebACLMigrationStack,
    CreateWebACLMigrationStack,

    -- * Request Lenses
    cwamsWebACLId,
    cwamsS3BucketName,
    cwamsIgnoreUnsupportedType,

    -- * Destructuring the Response
    createWebACLMigrationStackResponse,
    CreateWebACLMigrationStackResponse,

    -- * Response Lenses
    cwamsrsResponseStatus,
    cwamsrsS3ObjectURL,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types

-- | /See:/ 'createWebACLMigrationStack' smart constructor.
data CreateWebACLMigrationStack = CreateWebACLMigrationStack'
  { _cwamsWebACLId ::
      !Text,
    _cwamsS3BucketName :: !Text,
    _cwamsIgnoreUnsupportedType :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWebACLMigrationStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwamsWebACLId' - The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
--
-- * 'cwamsS3BucketName' - The name of the Amazon S3 bucket to store the CloudFormation template in. The S3 bucket must be configured as follows for the migration:      * The bucket name must start with @aws-waf-migration-@ . For example, @aws-waf-migration-my-web-acl@ .     * The bucket must be in the Region where you are deploying the template. For example, for a web ACL in us-west-2, you must use an Amazon S3 bucket in us-west-2 and you must deploy the template stack to us-west-2.      * The bucket policies must permit the migration process to write data. For listings of the bucket policies, see the Examples section.
--
-- * 'cwamsIgnoreUnsupportedType' - Indicates whether to exclude entities that can't be migrated or to stop the migration. Set this to true to ignore unsupported entities in the web ACL during the migration. Otherwise, if AWS WAF encounters unsupported entities, it stops the process and throws an exception.
createWebACLMigrationStack ::
  -- | 'cwamsWebACLId'
  Text ->
  -- | 'cwamsS3BucketName'
  Text ->
  -- | 'cwamsIgnoreUnsupportedType'
  Bool ->
  CreateWebACLMigrationStack
createWebACLMigrationStack
  pWebACLId_
  pS3BucketName_
  pIgnoreUnsupportedType_ =
    CreateWebACLMigrationStack'
      { _cwamsWebACLId = pWebACLId_,
        _cwamsS3BucketName = pS3BucketName_,
        _cwamsIgnoreUnsupportedType = pIgnoreUnsupportedType_
      }

-- | The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
cwamsWebACLId :: Lens' CreateWebACLMigrationStack Text
cwamsWebACLId = lens _cwamsWebACLId (\s a -> s {_cwamsWebACLId = a})

-- | The name of the Amazon S3 bucket to store the CloudFormation template in. The S3 bucket must be configured as follows for the migration:      * The bucket name must start with @aws-waf-migration-@ . For example, @aws-waf-migration-my-web-acl@ .     * The bucket must be in the Region where you are deploying the template. For example, for a web ACL in us-west-2, you must use an Amazon S3 bucket in us-west-2 and you must deploy the template stack to us-west-2.      * The bucket policies must permit the migration process to write data. For listings of the bucket policies, see the Examples section.
cwamsS3BucketName :: Lens' CreateWebACLMigrationStack Text
cwamsS3BucketName = lens _cwamsS3BucketName (\s a -> s {_cwamsS3BucketName = a})

-- | Indicates whether to exclude entities that can't be migrated or to stop the migration. Set this to true to ignore unsupported entities in the web ACL during the migration. Otherwise, if AWS WAF encounters unsupported entities, it stops the process and throws an exception.
cwamsIgnoreUnsupportedType :: Lens' CreateWebACLMigrationStack Bool
cwamsIgnoreUnsupportedType = lens _cwamsIgnoreUnsupportedType (\s a -> s {_cwamsIgnoreUnsupportedType = a})

instance AWSRequest CreateWebACLMigrationStack where
  type
    Rs CreateWebACLMigrationStack =
      CreateWebACLMigrationStackResponse
  request = postJSON waf
  response =
    receiveJSON
      ( \s h x ->
          CreateWebACLMigrationStackResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "S3ObjectUrl")
      )

instance Hashable CreateWebACLMigrationStack

instance NFData CreateWebACLMigrationStack

instance ToHeaders CreateWebACLMigrationStack where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSWAF_20150824.CreateWebACLMigrationStack" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateWebACLMigrationStack where
  toJSON CreateWebACLMigrationStack' {..} =
    object
      ( catMaybes
          [ Just ("WebACLId" .= _cwamsWebACLId),
            Just ("S3BucketName" .= _cwamsS3BucketName),
            Just ("IgnoreUnsupportedType" .= _cwamsIgnoreUnsupportedType)
          ]
      )

instance ToPath CreateWebACLMigrationStack where
  toPath = const "/"

instance ToQuery CreateWebACLMigrationStack where
  toQuery = const mempty

-- | /See:/ 'createWebACLMigrationStackResponse' smart constructor.
data CreateWebACLMigrationStackResponse = CreateWebACLMigrationStackResponse'
  { _cwamsrsResponseStatus ::
      !Int,
    _cwamsrsS3ObjectURL ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWebACLMigrationStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwamsrsResponseStatus' - -- | The response status code.
--
-- * 'cwamsrsS3ObjectURL' - The URL of the template created in Amazon S3.
createWebACLMigrationStackResponse ::
  -- | 'cwamsrsResponseStatus'
  Int ->
  -- | 'cwamsrsS3ObjectURL'
  Text ->
  CreateWebACLMigrationStackResponse
createWebACLMigrationStackResponse pResponseStatus_ pS3ObjectURL_ =
  CreateWebACLMigrationStackResponse'
    { _cwamsrsResponseStatus =
        pResponseStatus_,
      _cwamsrsS3ObjectURL = pS3ObjectURL_
    }

-- | -- | The response status code.
cwamsrsResponseStatus :: Lens' CreateWebACLMigrationStackResponse Int
cwamsrsResponseStatus = lens _cwamsrsResponseStatus (\s a -> s {_cwamsrsResponseStatus = a})

-- | The URL of the template created in Amazon S3.
cwamsrsS3ObjectURL :: Lens' CreateWebACLMigrationStackResponse Text
cwamsrsS3ObjectURL = lens _cwamsrsS3ObjectURL (\s a -> s {_cwamsrsS3ObjectURL = a})

instance NFData CreateWebACLMigrationStackResponse
