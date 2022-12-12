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
-- Module      : Amazonka.Evidently.CreateSegment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to define a /segment/ of your audience. A segment is
-- a portion of your audience that share one or more characteristics.
-- Examples could be Chrome browser users, users in Europe, or Firefox
-- browser users in Europe who also fit other criteria that your
-- application collects, such as age.
--
-- Using a segment in an experiment limits that experiment to evaluate only
-- the users who match the segment criteria. Using one or more segments in
-- a launch allows you to define different traffic splits for the different
-- audience segments.
--
-- >  <p>For more information about segment pattern syntax, see <a href="https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html#CloudWatch-Evidently-segments-syntax.html"> Segment rule pattern syntax</a>.</p> <p>The pattern that you define for a segment is matched against the value of <code>evaluationContext</code>, which is passed into Evidently in the <a href="https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html">EvaluateFeature</a> operation, when Evidently assigns a feature variation to a user.</p>
module Amazonka.Evidently.CreateSegment
  ( -- * Creating a Request
    CreateSegment (..),
    newCreateSegment,

    -- * Request Lenses
    createSegment_description,
    createSegment_tags,
    createSegment_name,
    createSegment_pattern,

    -- * Destructuring the Response
    CreateSegmentResponse (..),
    newCreateSegmentResponse,

    -- * Response Lenses
    createSegmentResponse_httpStatus,
    createSegmentResponse_segment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSegment' smart constructor.
data CreateSegment = CreateSegment'
  { -- | An optional description for this segment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Assigns one or more tags (key-value pairs) to the segment.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- Tags don\'t have any semantic meaning to Amazon Web Services and are
    -- interpreted strictly as strings of characters.
    --
    -- >  <p>You can associate as many as 50 tags with a segment.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the segment.
    name :: Prelude.Text,
    -- | The pattern to use for the segment. For more information about pattern
    -- syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html#CloudWatch-Evidently-segments-syntax.html Segment rule pattern syntax>.
    pattern' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSegment_description' - An optional description for this segment.
--
-- 'tags', 'createSegment_tags' - Assigns one or more tags (key-value pairs) to the segment.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with a segment.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
--
-- 'name', 'createSegment_name' - A name for the segment.
--
-- 'pattern'', 'createSegment_pattern' - The pattern to use for the segment. For more information about pattern
-- syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html#CloudWatch-Evidently-segments-syntax.html Segment rule pattern syntax>.
newCreateSegment ::
  -- | 'name'
  Prelude.Text ->
  -- | 'pattern''
  Prelude.Text ->
  CreateSegment
newCreateSegment pName_ pPattern_ =
  CreateSegment'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      pattern' = pPattern_
    }

-- | An optional description for this segment.
createSegment_description :: Lens.Lens' CreateSegment (Prelude.Maybe Prelude.Text)
createSegment_description = Lens.lens (\CreateSegment' {description} -> description) (\s@CreateSegment' {} a -> s {description = a} :: CreateSegment)

-- | Assigns one or more tags (key-value pairs) to the segment.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with a segment.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
createSegment_tags :: Lens.Lens' CreateSegment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSegment_tags = Lens.lens (\CreateSegment' {tags} -> tags) (\s@CreateSegment' {} a -> s {tags = a} :: CreateSegment) Prelude.. Lens.mapping Lens.coerced

-- | A name for the segment.
createSegment_name :: Lens.Lens' CreateSegment Prelude.Text
createSegment_name = Lens.lens (\CreateSegment' {name} -> name) (\s@CreateSegment' {} a -> s {name = a} :: CreateSegment)

-- | The pattern to use for the segment. For more information about pattern
-- syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-segments.html#CloudWatch-Evidently-segments-syntax.html Segment rule pattern syntax>.
createSegment_pattern :: Lens.Lens' CreateSegment Prelude.Text
createSegment_pattern = Lens.lens (\CreateSegment' {pattern'} -> pattern') (\s@CreateSegment' {} a -> s {pattern' = a} :: CreateSegment)

instance Core.AWSRequest CreateSegment where
  type
    AWSResponse CreateSegment =
      CreateSegmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSegmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "segment")
      )

instance Prelude.Hashable CreateSegment where
  hashWithSalt _salt CreateSegment' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData CreateSegment where
  rnf CreateSegment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pattern'

instance Data.ToHeaders CreateSegment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSegment where
  toJSON CreateSegment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("pattern" Data..= pattern')
          ]
      )

instance Data.ToPath CreateSegment where
  toPath = Prelude.const "/segments"

instance Data.ToQuery CreateSegment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSegmentResponse' smart constructor.
data CreateSegmentResponse = CreateSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains the complete information about the segment
    -- that was just created.
    segment :: Segment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segment', 'createSegmentResponse_segment' - A structure that contains the complete information about the segment
-- that was just created.
newCreateSegmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'segment'
  Segment ->
  CreateSegmentResponse
newCreateSegmentResponse pHttpStatus_ pSegment_ =
  CreateSegmentResponse'
    { httpStatus = pHttpStatus_,
      segment = pSegment_
    }

-- | The response's http status code.
createSegmentResponse_httpStatus :: Lens.Lens' CreateSegmentResponse Prelude.Int
createSegmentResponse_httpStatus = Lens.lens (\CreateSegmentResponse' {httpStatus} -> httpStatus) (\s@CreateSegmentResponse' {} a -> s {httpStatus = a} :: CreateSegmentResponse)

-- | A structure that contains the complete information about the segment
-- that was just created.
createSegmentResponse_segment :: Lens.Lens' CreateSegmentResponse Segment
createSegmentResponse_segment = Lens.lens (\CreateSegmentResponse' {segment} -> segment) (\s@CreateSegmentResponse' {} a -> s {segment = a} :: CreateSegmentResponse)

instance Prelude.NFData CreateSegmentResponse where
  rnf CreateSegmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segment
