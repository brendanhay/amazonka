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
-- Module      : Amazonka.ApplicationAutoScaling.TagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or edits tags on an Application Auto Scaling scalable target.
--
-- Each tag consists of a tag key and a tag value, which are both
-- case-sensitive strings. To add a tag, specify a new tag key and a tag
-- value. To edit a tag, specify an existing tag key and a new tag value.
--
-- You can use this operation to tag an Application Auto Scaling scalable
-- target, but you cannot tag a scaling policy or scheduled action.
--
-- You can also add tags to an Application Auto Scaling scalable target
-- while creating it (@RegisterScalableTarget@).
--
-- For general information about tags, including the format and syntax, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
--
-- Use tags to control access to a scalable target. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/application/userguide/resource-tagging-support.html Tagging support for Application Auto Scaling>
-- in the /Application Auto Scaling User Guide/.
module Amazonka.ApplicationAutoScaling.TagResource
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

import Amazonka.ApplicationAutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagResource' smart constructor.
data TagResource = TagResource'
  { -- | Identifies the Application Auto Scaling scalable target that you want to
    -- apply tags to.
    --
    -- For example:
    -- @arn:aws:application-autoscaling:us-east-1:123456789012:scalable-target\/1234abcd56ab78cd901ef1234567890ab123@
    --
    -- To get the ARN for a scalable target, use DescribeScalableTargets.
    resourceARN :: Prelude.Text,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource.
    --
    -- Each tag consists of a tag key and a tag value.
    --
    -- You cannot have more than one tag on an Application Auto Scaling
    -- scalable target with the same tag key. If you specify an existing tag
    -- key with a different tag value, Application Auto Scaling replaces the
    -- current tag value with the specified one.
    --
    -- For information about the rules that apply to tag keys and tag values,
    -- see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-defined tag restrictions>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
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
-- 'resourceARN', 'tagResource_resourceARN' - Identifies the Application Auto Scaling scalable target that you want to
-- apply tags to.
--
-- For example:
-- @arn:aws:application-autoscaling:us-east-1:123456789012:scalable-target\/1234abcd56ab78cd901ef1234567890ab123@
--
-- To get the ARN for a scalable target, use DescribeScalableTargets.
--
-- 'tags', 'tagResource_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource.
--
-- Each tag consists of a tag key and a tag value.
--
-- You cannot have more than one tag on an Application Auto Scaling
-- scalable target with the same tag key. If you specify an existing tag
-- key with a different tag value, Application Auto Scaling replaces the
-- current tag value with the specified one.
--
-- For information about the rules that apply to tag keys and tag values,
-- see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-defined tag restrictions>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
newTagResource ::
  -- | 'resourceARN'
  Prelude.Text ->
  TagResource
newTagResource pResourceARN_ =
  TagResource'
    { resourceARN = pResourceARN_,
      tags = Prelude.mempty
    }

-- | Identifies the Application Auto Scaling scalable target that you want to
-- apply tags to.
--
-- For example:
-- @arn:aws:application-autoscaling:us-east-1:123456789012:scalable-target\/1234abcd56ab78cd901ef1234567890ab123@
--
-- To get the ARN for a scalable target, use DescribeScalableTargets.
tagResource_resourceARN :: Lens.Lens' TagResource Prelude.Text
tagResource_resourceARN = Lens.lens (\TagResource' {resourceARN} -> resourceARN) (\s@TagResource' {} a -> s {resourceARN = a} :: TagResource)

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource.
--
-- Each tag consists of a tag key and a tag value.
--
-- You cannot have more than one tag on an Application Auto Scaling
-- scalable target with the same tag key. If you specify an existing tag
-- key with a different tag value, Application Auto Scaling replaces the
-- current tag value with the specified one.
--
-- For information about the rules that apply to tag keys and tag values,
-- see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-defined tag restrictions>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
tagResource_tags :: Lens.Lens' TagResource (Prelude.HashMap Prelude.Text Prelude.Text)
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
              Data.=# ( "AnyScaleFrontendService.TagResource" ::
                          Prelude.ByteString
                      ),
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
