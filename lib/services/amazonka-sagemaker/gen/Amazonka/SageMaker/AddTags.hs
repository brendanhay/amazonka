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
-- Module      : Amazonka.SageMaker.AddTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified SageMaker
-- resource. You can add tags to notebook instances, training jobs,
-- hyperparameter tuning jobs, batch transform jobs, models, labeling jobs,
-- work teams, endpoint configurations, and endpoints.
--
-- Each tag consists of a key and an optional value. Tag keys must be
-- unique per resource. For more information about tags, see For more
-- information, see
-- <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ Amazon Web Services Tagging Strategies>.
--
-- Tags that you add to a hyperparameter tuning job by calling this API are
-- also added to any training jobs that the hyperparameter tuning job
-- launches after you call this API, but not to training jobs that the
-- hyperparameter tuning job launched before you called this API. To make
-- sure that the tags associated with a hyperparameter tuning job are also
-- added to all training jobs that the hyperparameter tuning job launches,
-- add the tags when you first create the tuning job by specifying them in
-- the @Tags@ parameter of CreateHyperParameterTuningJob
--
-- Tags that you add to a SageMaker Studio Domain or User Profile by
-- calling this API are also added to any Apps that the Domain or User
-- Profile launches after you call this API, but not to Apps that the
-- Domain or User Profile launched before you called this API. To make sure
-- that the tags associated with a Domain or User Profile are also added to
-- all Apps that the Domain or User Profile launches, add the tags when you
-- first create the Domain or User Profile by specifying them in the @Tags@
-- parameter of CreateDomain or CreateUserProfile.
module Amazonka.SageMaker.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_resourceArn,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_tags,
    addTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to tag.
    resourceArn :: Prelude.Text,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'addTags_resourceArn' - The Amazon Resource Name (ARN) of the resource that you want to tag.
--
-- 'tags', 'addTags_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
newAddTags ::
  -- | 'resourceArn'
  Prelude.Text ->
  AddTags
newAddTags pResourceArn_ =
  AddTags'
    { resourceArn = pResourceArn_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource that you want to tag.
addTags_resourceArn :: Lens.Lens' AddTags Prelude.Text
addTags_resourceArn = Lens.lens (\AddTags' {resourceArn} -> resourceArn) (\s@AddTags' {} a -> s {resourceArn = a} :: AddTags)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
addTags_tags :: Lens.Lens' AddTags [Tag]
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Prelude.. Lens.coerced

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTags where
  hashWithSalt _salt AddTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AddTags where
  rnf AddTags' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders AddTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.AddTags" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddTags where
  toJSON AddTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath AddTags where
  toPath = Prelude.const "/"

instance Data.ToQuery AddTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | A list of tags associated with the SageMaker resource.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'addTagsResponse_tags' - A list of tags associated with the SageMaker resource.
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tags associated with the SageMaker resource.
addTagsResponse_tags :: Lens.Lens' AddTagsResponse (Prelude.Maybe [Tag])
addTagsResponse_tags = Lens.lens (\AddTagsResponse' {tags} -> tags) (\s@AddTagsResponse' {} a -> s {tags = a} :: AddTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Prelude.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Prelude.NFData AddTagsResponse where
  rnf AddTagsResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
