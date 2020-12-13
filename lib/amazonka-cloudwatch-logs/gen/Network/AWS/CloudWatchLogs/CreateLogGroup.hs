{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CreateLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a log group with the specified name. You can create up to 20,000 log groups per account.
--
-- You must use the following guidelines when naming a log group:
--
--     * Log group names must be unique within a region for an AWS account.
--
--
--     * Log group names can be between 1 and 512 characters long.
--
--
--     * Log group names consist of the following characters: a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), '.' (period), and '#' (number sign)
--
--
-- When you create a log group, by default the log events in the log group never expire. To set a retention policy so that events expire and are deleted after a specified time, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutRetentionPolicy.html PutRetentionPolicy> .
-- If you associate a AWS Key Management Service (AWS KMS) customer master key (CMK) with the log group, ingested data is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.
-- If you attempt to associate a CMK with the log group but the CMK does not exist or the CMK is disabled, you receive an @InvalidParameterException@ error.
-- /Important:/ CloudWatch Logs supports only symmetric CMKs. Do not associate an asymmetric CMK with your log group. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
module Network.AWS.CloudWatchLogs.CreateLogGroup
  ( -- * Creating a request
    CreateLogGroup (..),
    mkCreateLogGroup,

    -- ** Request lenses
    clgLogGroupName,
    clgKmsKeyId,
    clgTags,

    -- * Destructuring the response
    CreateLogGroupResponse (..),
    mkCreateLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLogGroup' smart constructor.
data CreateLogGroup = CreateLogGroup'
  { -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The key-value pairs to use for the tags.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLogGroup' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
-- * 'tags' - The key-value pairs to use for the tags.
mkCreateLogGroup ::
  -- | 'logGroupName'
  Lude.Text ->
  CreateLogGroup
mkCreateLogGroup pLogGroupName_ =
  CreateLogGroup'
    { logGroupName = pLogGroupName_,
      kmsKeyId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgLogGroupName :: Lens.Lens' CreateLogGroup Lude.Text
clgLogGroupName = Lens.lens (logGroupName :: CreateLogGroup -> Lude.Text) (\s a -> s {logGroupName = a} :: CreateLogGroup)
{-# DEPRECATED clgLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgKmsKeyId :: Lens.Lens' CreateLogGroup (Lude.Maybe Lude.Text)
clgKmsKeyId = Lens.lens (kmsKeyId :: CreateLogGroup -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateLogGroup)
{-# DEPRECATED clgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The key-value pairs to use for the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clgTags :: Lens.Lens' CreateLogGroup (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
clgTags = Lens.lens (tags :: CreateLogGroup -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateLogGroup)
{-# DEPRECATED clgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateLogGroup where
  type Rs CreateLogGroup = CreateLogGroupResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull CreateLogGroupResponse'

instance Lude.ToHeaders CreateLogGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.CreateLogGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLogGroup where
  toJSON CreateLogGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            ("kmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateLogGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLogGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLogGroupResponse' smart constructor.
data CreateLogGroupResponse = CreateLogGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLogGroupResponse' with the minimum fields required to make a request.
mkCreateLogGroupResponse ::
  CreateLogGroupResponse
mkCreateLogGroupResponse = CreateLogGroupResponse'
