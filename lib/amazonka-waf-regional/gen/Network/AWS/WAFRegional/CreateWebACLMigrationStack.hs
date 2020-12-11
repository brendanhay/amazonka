{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateWebACLMigrationStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation WAFV2 template for the specified web ACL in the specified Amazon S3 bucket. Then, in CloudFormation, you create a stack from the template, to create the web ACL and its resources in AWS WAFV2. Use this to migrate your AWS WAF Classic web ACL to the latest version of AWS WAF.
--
-- This is part of a larger migration procedure for web ACLs from AWS WAF Classic to the latest version of AWS WAF. For the full procedure, including caveats and manual steps to complete the migration and switch over to the new web ACL, see <https://docs.aws.amazon.com/waf/latest/developerguide/waf-migrating-from-classic.html Migrating your AWS WAF Classic resources to AWS WAF> in the <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateWebACLMigrationStack
  ( -- * Creating a request
    CreateWebACLMigrationStack (..),
    mkCreateWebACLMigrationStack,

    -- ** Request lenses
    cwamsWebACLId,
    cwamsS3BucketName,
    cwamsIgnoreUnsupportedType,

    -- * Destructuring the response
    CreateWebACLMigrationStackResponse (..),
    mkCreateWebACLMigrationStackResponse,

    -- ** Response lenses
    cwamsrsResponseStatus,
    cwamsrsS3ObjectURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkCreateWebACLMigrationStack' smart constructor.
data CreateWebACLMigrationStack = CreateWebACLMigrationStack'
  { webACLId ::
      Lude.Text,
    s3BucketName :: Lude.Text,
    ignoreUnsupportedType :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWebACLMigrationStack' with the minimum fields required to make a request.
--
-- * 'ignoreUnsupportedType' - Indicates whether to exclude entities that can't be migrated or to stop the migration. Set this to true to ignore unsupported entities in the web ACL during the migration. Otherwise, if AWS WAF encounters unsupported entities, it stops the process and throws an exception.
-- * 's3BucketName' - The name of the Amazon S3 bucket to store the CloudFormation template in. The S3 bucket must be configured as follows for the migration:
--
--
--     * The bucket name must start with @aws-waf-migration-@ . For example, @aws-waf-migration-my-web-acl@ .
--
--
--     * The bucket must be in the Region where you are deploying the template. For example, for a web ACL in us-west-2, you must use an Amazon S3 bucket in us-west-2 and you must deploy the template stack to us-west-2.
--
--
--     * The bucket policies must permit the migration process to write data. For listings of the bucket policies, see the Examples section.
--
--
-- * 'webACLId' - The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
mkCreateWebACLMigrationStack ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 's3BucketName'
  Lude.Text ->
  -- | 'ignoreUnsupportedType'
  Lude.Bool ->
  CreateWebACLMigrationStack
mkCreateWebACLMigrationStack
  pWebACLId_
  pS3BucketName_
  pIgnoreUnsupportedType_ =
    CreateWebACLMigrationStack'
      { webACLId = pWebACLId_,
        s3BucketName = pS3BucketName_,
        ignoreUnsupportedType = pIgnoreUnsupportedType_
      }

-- | The UUID of the WAF Classic web ACL that you want to migrate to WAF v2.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwamsWebACLId :: Lens.Lens' CreateWebACLMigrationStack Lude.Text
cwamsWebACLId = Lens.lens (webACLId :: CreateWebACLMigrationStack -> Lude.Text) (\s a -> s {webACLId = a} :: CreateWebACLMigrationStack)
{-# DEPRECATED cwamsWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The name of the Amazon S3 bucket to store the CloudFormation template in. The S3 bucket must be configured as follows for the migration:
--
--
--     * The bucket name must start with @aws-waf-migration-@ . For example, @aws-waf-migration-my-web-acl@ .
--
--
--     * The bucket must be in the Region where you are deploying the template. For example, for a web ACL in us-west-2, you must use an Amazon S3 bucket in us-west-2 and you must deploy the template stack to us-west-2.
--
--
--     * The bucket policies must permit the migration process to write data. For listings of the bucket policies, see the Examples section.
--
--
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwamsS3BucketName :: Lens.Lens' CreateWebACLMigrationStack Lude.Text
cwamsS3BucketName = Lens.lens (s3BucketName :: CreateWebACLMigrationStack -> Lude.Text) (\s a -> s {s3BucketName = a} :: CreateWebACLMigrationStack)
{-# DEPRECATED cwamsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | Indicates whether to exclude entities that can't be migrated or to stop the migration. Set this to true to ignore unsupported entities in the web ACL during the migration. Otherwise, if AWS WAF encounters unsupported entities, it stops the process and throws an exception.
--
-- /Note:/ Consider using 'ignoreUnsupportedType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwamsIgnoreUnsupportedType :: Lens.Lens' CreateWebACLMigrationStack Lude.Bool
cwamsIgnoreUnsupportedType = Lens.lens (ignoreUnsupportedType :: CreateWebACLMigrationStack -> Lude.Bool) (\s a -> s {ignoreUnsupportedType = a} :: CreateWebACLMigrationStack)
{-# DEPRECATED cwamsIgnoreUnsupportedType "Use generic-lens or generic-optics with 'ignoreUnsupportedType' instead." #-}

instance Lude.AWSRequest CreateWebACLMigrationStack where
  type
    Rs CreateWebACLMigrationStack =
      CreateWebACLMigrationStackResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWebACLMigrationStackResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "S3ObjectUrl")
      )

instance Lude.ToHeaders CreateWebACLMigrationStack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.CreateWebACLMigrationStack" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWebACLMigrationStack where
  toJSON CreateWebACLMigrationStack' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WebACLId" Lude..= webACLId),
            Lude.Just ("S3BucketName" Lude..= s3BucketName),
            Lude.Just ("IgnoreUnsupportedType" Lude..= ignoreUnsupportedType)
          ]
      )

instance Lude.ToPath CreateWebACLMigrationStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWebACLMigrationStack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWebACLMigrationStackResponse' smart constructor.
data CreateWebACLMigrationStackResponse = CreateWebACLMigrationStackResponse'
  { responseStatus ::
      Lude.Int,
    s3ObjectURL ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWebACLMigrationStackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 's3ObjectURL' - The URL of the template created in Amazon S3.
mkCreateWebACLMigrationStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 's3ObjectURL'
  Lude.Text ->
  CreateWebACLMigrationStackResponse
mkCreateWebACLMigrationStackResponse pResponseStatus_ pS3ObjectURL_ =
  CreateWebACLMigrationStackResponse'
    { responseStatus =
        pResponseStatus_,
      s3ObjectURL = pS3ObjectURL_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwamsrsResponseStatus :: Lens.Lens' CreateWebACLMigrationStackResponse Lude.Int
cwamsrsResponseStatus = Lens.lens (responseStatus :: CreateWebACLMigrationStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWebACLMigrationStackResponse)
{-# DEPRECATED cwamsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The URL of the template created in Amazon S3.
--
-- /Note:/ Consider using 's3ObjectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwamsrsS3ObjectURL :: Lens.Lens' CreateWebACLMigrationStackResponse Lude.Text
cwamsrsS3ObjectURL = Lens.lens (s3ObjectURL :: CreateWebACLMigrationStackResponse -> Lude.Text) (\s a -> s {s3ObjectURL = a} :: CreateWebACLMigrationStackResponse)
{-# DEPRECATED cwamsrsS3ObjectURL "Use generic-lens or generic-optics with 's3ObjectURL' instead." #-}
