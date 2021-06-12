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
-- Module      : Network.AWS.CloudWatchLogs.CreateLogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a log group with the specified name. You can create up to 20,000
-- log groups per account.
--
-- You must use the following guidelines when naming a log group:
--
-- -   Log group names must be unique within a region for an AWS account.
--
-- -   Log group names can be between 1 and 512 characters long.
--
-- -   Log group names consist of the following characters: a-z, A-Z, 0-9,
--     \'_\' (underscore), \'-\' (hyphen), \'\/\' (forward slash), \'.\'
--     (period), and \'#\' (number sign)
--
-- When you create a log group, by default the log events in the log group
-- never expire. To set a retention policy so that events expire and are
-- deleted after a specified time, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutRetentionPolicy.html PutRetentionPolicy>.
--
-- If you associate a AWS Key Management Service (AWS KMS) customer master
-- key (CMK) with the log group, ingested data is encrypted using the CMK.
-- This association is stored as long as the data encrypted with the CMK is
-- still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs
-- to decrypt this data whenever it is requested.
--
-- If you attempt to associate a CMK with the log group but the CMK does
-- not exist or the CMK is disabled, you receive an
-- @InvalidParameterException@ error.
--
-- CloudWatch Logs supports only symmetric CMKs. Do not associate an
-- asymmetric CMK with your log group. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
module Network.AWS.CloudWatchLogs.CreateLogGroup
  ( -- * Creating a Request
    CreateLogGroup (..),
    newCreateLogGroup,

    -- * Request Lenses
    createLogGroup_kmsKeyId,
    createLogGroup_tags,
    createLogGroup_logGroupName,

    -- * Destructuring the Response
    CreateLogGroupResponse (..),
    newCreateLogGroupResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLogGroup' smart constructor.
data CreateLogGroup = CreateLogGroup'
  { -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
    -- data. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The key-value pairs to use for the tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the log group.
    logGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'createLogGroup_kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>.
--
-- 'tags', 'createLogGroup_tags' - The key-value pairs to use for the tags.
--
-- 'logGroupName', 'createLogGroup_logGroupName' - The name of the log group.
newCreateLogGroup ::
  -- | 'logGroupName'
  Core.Text ->
  CreateLogGroup
newCreateLogGroup pLogGroupName_ =
  CreateLogGroup'
    { kmsKeyId = Core.Nothing,
      tags = Core.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>.
createLogGroup_kmsKeyId :: Lens.Lens' CreateLogGroup (Core.Maybe Core.Text)
createLogGroup_kmsKeyId = Lens.lens (\CreateLogGroup' {kmsKeyId} -> kmsKeyId) (\s@CreateLogGroup' {} a -> s {kmsKeyId = a} :: CreateLogGroup)

-- | The key-value pairs to use for the tags.
createLogGroup_tags :: Lens.Lens' CreateLogGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
createLogGroup_tags = Lens.lens (\CreateLogGroup' {tags} -> tags) (\s@CreateLogGroup' {} a -> s {tags = a} :: CreateLogGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the log group.
createLogGroup_logGroupName :: Lens.Lens' CreateLogGroup Core.Text
createLogGroup_logGroupName = Lens.lens (\CreateLogGroup' {logGroupName} -> logGroupName) (\s@CreateLogGroup' {} a -> s {logGroupName = a} :: CreateLogGroup)

instance Core.AWSRequest CreateLogGroup where
  type
    AWSResponse CreateLogGroup =
      CreateLogGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull CreateLogGroupResponse'

instance Core.Hashable CreateLogGroup

instance Core.NFData CreateLogGroup

instance Core.ToHeaders CreateLogGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.CreateLogGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateLogGroup where
  toJSON CreateLogGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("tags" Core..=) Core.<$> tags,
            Core.Just ("logGroupName" Core..= logGroupName)
          ]
      )

instance Core.ToPath CreateLogGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateLogGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateLogGroupResponse' smart constructor.
data CreateLogGroupResponse = CreateLogGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateLogGroupResponse ::
  CreateLogGroupResponse
newCreateLogGroupResponse = CreateLogGroupResponse'

instance Core.NFData CreateLogGroupResponse
