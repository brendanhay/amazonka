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
-- Module      : Network.AWS.GameLift.TagResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a tag to a GameLift resource. AWS resource tags provide an
-- additional management tool set. You can use tags to organize resources,
-- create IAM permissions policies to manage access to groups of resources,
-- customize AWS cost breakdowns, etc. This operation handles the
-- permissions necessary to manage tags for the following GameLift resource
-- types:
--
-- -   Build
--
-- -   Script
--
-- -   Fleet
--
-- -   Alias
--
-- -   GameSessionQueue
--
-- -   MatchmakingConfiguration
--
-- -   MatchmakingRuleSet
--
-- To add a tag to a resource, specify the unique ARN value for the
-- resource and provide a tag list containing one or more tags. The
-- operation succeeds even if the list includes tags that are already
-- assigned to the specified resource.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/
--
-- <http://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies>
--
-- __Related operations__
--
-- -   TagResource
--
-- -   UntagResource
--
-- -   ListTagsForResource
module Network.AWS.GameLift.TagResource
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to and uniquely identifies the GameLift resource that
    -- you want to assign tags to. GameLift resource ARNs are included in the
    -- data object for the resource, which can be retrieved by calling a List
    -- or Describe operation for the resource type.
    resourceARN :: Prelude.Text,
    -- | A list of one or more tags to assign to the specified GameLift resource.
    -- Tags are developer-defined and structured as key-value pairs. The
    -- maximum tag limit may be lower than stated. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- for actual tagging limits.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- that is assigned to and uniquely identifies the GameLift resource that
-- you want to assign tags to. GameLift resource ARNs are included in the
-- data object for the resource, which can be retrieved by calling a List
-- or Describe operation for the resource type.
--
-- 'tags', 'tagResource_tags' - A list of one or more tags to assign to the specified GameLift resource.
-- Tags are developer-defined and structured as key-value pairs. The
-- maximum tag limit may be lower than stated. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for actual tagging limits.
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
-- that is assigned to and uniquely identifies the GameLift resource that
-- you want to assign tags to. GameLift resource ARNs are included in the
-- data object for the resource, which can be retrieved by calling a List
-- or Describe operation for the resource type.
tagResource_resourceARN :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceARN = Lens.lens (\TagResource' {resourceARN} -> resourceARN) (\s@TagResource' {} a -> s {resourceARN = a} :: TagResource)

-- | A list of one or more tags to assign to the specified GameLift resource.
-- Tags are developer-defined and structured as key-value pairs. The
-- maximum tag limit may be lower than stated. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- for actual tagging limits.
tagResource_tags :: Lens.Lens' TagResource [Tag]
tagResource_tags = Lens.lens (\TagResource' {tags} -> tags) (\s@TagResource' {} a -> s {tags = a} :: TagResource) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagResource

instance Prelude.NFData TagResource

instance Prelude.ToHeaders TagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.TagResource" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Prelude..= resourceARN),
            Prelude.Just ("Tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath TagResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData TagResourceResponse
