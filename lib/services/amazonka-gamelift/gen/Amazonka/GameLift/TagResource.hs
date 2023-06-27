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
-- Module      : Amazonka.GameLift.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a tag to an Amazon GameLift resource. You can use tags to
-- organize resources, create IAM permissions policies to manage access to
-- groups of resources, customize Amazon Web Services cost breakdowns, and
-- more. This operation handles the permissions necessary to manage tags
-- for Amazon GameLift resources that support tagging.
--
-- To add a tag to a resource, specify the unique ARN value for the
-- resource and provide a tag list containing one or more tags. The
-- operation succeeds even if the list includes tags that are already
-- assigned to the resource.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/
--
-- <http://aws.amazon.com/answers/account-management/aws-tagging-strategies/ Amazon Web Services Tagging Strategies>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.TagResource
  ( -- * Creating a Request
    TagResource (..),
    newTagResource,

    -- * Request Lenses
    tagResource_resourceARN,
    tagResource_tags,

    -- * Destructuring the Response
    TagResourceResponse (..),
    newTagResourceResponse,

    -- * Response Lenses
    tagResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that uniquely identifies the Amazon GameLift resource that you want to
    -- assign tags to. Amazon GameLift includes resource ARNs in the data
    -- object for the resource. You can retrieve the ARN by calling a @List@ or
    -- @Describe@ operation for the resource type.
    resourceARN :: Prelude.Text,
    -- | A list of one or more tags to assign to the specified Amazon GameLift
    -- resource. Tags are developer-defined and structured as key-value pairs.
    -- The maximum tag limit may be lower than stated. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for tagging limits.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'tagResource_resourceARN' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that uniquely identifies the Amazon GameLift resource that you want to
-- assign tags to. Amazon GameLift includes resource ARNs in the data
-- object for the resource. You can retrieve the ARN by calling a @List@ or
-- @Describe@ operation for the resource type.
--
-- 'tags', 'tagResource_tags' - A list of one or more tags to assign to the specified Amazon GameLift
-- resource. Tags are developer-defined and structured as key-value pairs.
-- The maximum tag limit may be lower than stated. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for tagging limits.
newTagResource ::
  -- | 'resourceARN'
  Prelude.Text ->
  TagResource
newTagResource pResourceARN_ =
  TagResource'
    { resourceARN = pResourceARN_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that uniquely identifies the Amazon GameLift resource that you want to
-- assign tags to. Amazon GameLift includes resource ARNs in the data
-- object for the resource. You can retrieve the ARN by calling a @List@ or
-- @Describe@ operation for the resource type.
tagResource_resourceARN :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceARN = Lens.lens (\TagResource' {resourceARN} -> resourceARN) (\s@TagResource' {} a -> s {resourceARN = a} :: TagResource)

-- | A list of one or more tags to assign to the specified Amazon GameLift
-- resource. Tags are developer-defined and structured as key-value pairs.
-- The maximum tag limit may be lower than stated. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for tagging limits.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Lens.coerced

instance Core.AWSRequest TagResource where
  type AWSResponse TagResource = TagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResource where
  hashWithSalt _salt TagResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagResource where
  rnf TagResource' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.TagResource" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TagResource where
  toJSON TagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Data..= resourceARN),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath TagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'tagResourceResponse_httpStatus' - The response's http status code.
newTagResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TagResourceResponse
newTagResourceResponse pHttpStatus_ =
  TagResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
tagResourceResponse_httpStatus :: Lens.Lens' TagResourceResponse Prelude.Int
tagResourceResponse_httpStatus = Lens.lens (\TagResourceResponse' {httpStatus} -> httpStatus) (\s@TagResourceResponse' {} a -> s {httpStatus = a} :: TagResourceResponse)

instance Prelude.NFData TagResourceResponse where
  rnf TagResourceResponse' {..} = Prelude.rnf httpStatus
