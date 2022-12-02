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
-- Module      : Amazonka.CloudWatchLogs.CreateLogGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- -   Log group names must be unique within a region for an Amazon Web
--     Services account.
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
-- If you associate a Key Management Service customer master key (CMK) with
-- the log group, ingested data is encrypted using the CMK. This
-- association is stored as long as the data encrypted with the CMK is
-- still within CloudWatch Logs. This enables CloudWatch Logs to decrypt
-- this data whenever it is requested.
--
-- If you attempt to associate a CMK with the log group but the CMK does
-- not exist or the CMK is disabled, you receive an
-- @InvalidParameterException@ error.
--
-- CloudWatch Logs supports only symmetric CMKs. Do not associate an
-- asymmetric CMK with your log group. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
module Amazonka.CloudWatchLogs.CreateLogGroup
  ( -- * Creating a Request
    CreateLogGroup (..),
    newCreateLogGroup,

    -- * Request Lenses
    createLogGroup_tags,
    createLogGroup_kmsKeyId,
    createLogGroup_logGroupName,

    -- * Destructuring the Response
    CreateLogGroupResponse (..),
    newCreateLogGroupResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLogGroup' smart constructor.
data CreateLogGroup = CreateLogGroup'
  { -- | The key-value pairs to use for the tags.
    --
    -- CloudWatch Logs doesn’t support IAM policies that prevent users from
    -- assigning specified tags to log groups using the
    -- @aws:Resource\/key-name @ or @aws:TagKeys@ condition keys. For more
    -- information about using tags to control access, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
    -- data. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - Key Management Service>.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLogGroup_tags' - The key-value pairs to use for the tags.
--
-- CloudWatch Logs doesn’t support IAM policies that prevent users from
-- assigning specified tags to log groups using the
-- @aws:Resource\/key-name @ or @aws:TagKeys@ condition keys. For more
-- information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
--
-- 'kmsKeyId', 'createLogGroup_kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - Key Management Service>.
--
-- 'logGroupName', 'createLogGroup_logGroupName' - The name of the log group.
newCreateLogGroup ::
  -- | 'logGroupName'
  Prelude.Text ->
  CreateLogGroup
newCreateLogGroup pLogGroupName_ =
  CreateLogGroup'
    { tags = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The key-value pairs to use for the tags.
--
-- CloudWatch Logs doesn’t support IAM policies that prevent users from
-- assigning specified tags to log groups using the
-- @aws:Resource\/key-name @ or @aws:TagKeys@ condition keys. For more
-- information about using tags to control access, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Controlling access to Amazon Web Services resources using tags>.
createLogGroup_tags :: Lens.Lens' CreateLogGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLogGroup_tags = Lens.lens (\CreateLogGroup' {tags} -> tags) (\s@CreateLogGroup' {} a -> s {tags = a} :: CreateLogGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - Key Management Service>.
createLogGroup_kmsKeyId :: Lens.Lens' CreateLogGroup (Prelude.Maybe Prelude.Text)
createLogGroup_kmsKeyId = Lens.lens (\CreateLogGroup' {kmsKeyId} -> kmsKeyId) (\s@CreateLogGroup' {} a -> s {kmsKeyId = a} :: CreateLogGroup)

-- | The name of the log group.
createLogGroup_logGroupName :: Lens.Lens' CreateLogGroup Prelude.Text
createLogGroup_logGroupName = Lens.lens (\CreateLogGroup' {logGroupName} -> logGroupName) (\s@CreateLogGroup' {} a -> s {logGroupName = a} :: CreateLogGroup)

instance Core.AWSRequest CreateLogGroup where
  type
    AWSResponse CreateLogGroup =
      CreateLogGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull CreateLogGroupResponse'

instance Prelude.Hashable CreateLogGroup where
  hashWithSalt _salt CreateLogGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData CreateLogGroup where
  rnf CreateLogGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf logGroupName

instance Data.ToHeaders CreateLogGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.CreateLogGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLogGroup where
  toJSON CreateLogGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("logGroupName" Data..= logGroupName)
          ]
      )

instance Data.ToPath CreateLogGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLogGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLogGroupResponse' smart constructor.
data CreateLogGroupResponse = CreateLogGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLogGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateLogGroupResponse ::
  CreateLogGroupResponse
newCreateLogGroupResponse = CreateLogGroupResponse'

instance Prelude.NFData CreateLogGroupResponse where
  rnf _ = ()
