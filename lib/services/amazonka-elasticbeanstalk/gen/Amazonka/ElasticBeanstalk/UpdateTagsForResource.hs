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
-- Module      : Amazonka.ElasticBeanstalk.UpdateTagsForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the list of tags applied to an AWS Elastic Beanstalk resource.
-- Two lists can be passed: @TagsToAdd@ for tags to add or update, and
-- @TagsToRemove@.
--
-- Elastic Beanstalk supports tagging of all of its resources. For details
-- about resource tagging, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/applications-tagging-resources.html Tagging Application Resources>.
--
-- If you create a custom IAM user policy to control permission to this
-- operation, specify one of the following two virtual actions (or both)
-- instead of the API operation name:
--
-- [elasticbeanstalk:AddTags]
--     Controls permission to call @UpdateTagsForResource@ and pass a list
--     of tags to add in the @TagsToAdd@ parameter.
--
-- [elasticbeanstalk:RemoveTags]
--     Controls permission to call @UpdateTagsForResource@ and pass a list
--     of tag keys to remove in the @TagsToRemove@ parameter.
--
-- For details about creating a custom user policy, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies Creating a Custom User Policy>.
module Amazonka.ElasticBeanstalk.UpdateTagsForResource
  ( -- * Creating a Request
    UpdateTagsForResource (..),
    newUpdateTagsForResource,

    -- * Request Lenses
    updateTagsForResource_tagsToAdd,
    updateTagsForResource_tagsToRemove,
    updateTagsForResource_resourceArn,

    -- * Destructuring the Response
    UpdateTagsForResourceResponse (..),
    newUpdateTagsForResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTagsForResource' smart constructor.
data UpdateTagsForResource = UpdateTagsForResource'
  { -- | A list of tags to add or update. If a key of an existing tag is added,
    -- the tag\'s value is updated.
    --
    -- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
    tagsToAdd :: Prelude.Maybe [Tag],
    -- | A list of tag keys to remove. If a tag key doesn\'t exist, it is
    -- silently ignored.
    --
    -- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
    tagsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the resouce to be updated.
    --
    -- Must be the ARN of an Elastic Beanstalk resource.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagsToAdd', 'updateTagsForResource_tagsToAdd' - A list of tags to add or update. If a key of an existing tag is added,
-- the tag\'s value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
--
-- 'tagsToRemove', 'updateTagsForResource_tagsToRemove' - A list of tag keys to remove. If a tag key doesn\'t exist, it is
-- silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
--
-- 'resourceArn', 'updateTagsForResource_resourceArn' - The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
newUpdateTagsForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  UpdateTagsForResource
newUpdateTagsForResource pResourceArn_ =
  UpdateTagsForResource'
    { tagsToAdd = Prelude.Nothing,
      tagsToRemove = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | A list of tags to add or update. If a key of an existing tag is added,
-- the tag\'s value is updated.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
updateTagsForResource_tagsToAdd :: Lens.Lens' UpdateTagsForResource (Prelude.Maybe [Tag])
updateTagsForResource_tagsToAdd = Lens.lens (\UpdateTagsForResource' {tagsToAdd} -> tagsToAdd) (\s@UpdateTagsForResource' {} a -> s {tagsToAdd = a} :: UpdateTagsForResource) Prelude.. Lens.mapping Lens.coerced

-- | A list of tag keys to remove. If a tag key doesn\'t exist, it is
-- silently ignored.
--
-- Specify at least one of these parameters: @TagsToAdd@, @TagsToRemove@.
updateTagsForResource_tagsToRemove :: Lens.Lens' UpdateTagsForResource (Prelude.Maybe [Prelude.Text])
updateTagsForResource_tagsToRemove = Lens.lens (\UpdateTagsForResource' {tagsToRemove} -> tagsToRemove) (\s@UpdateTagsForResource' {} a -> s {tagsToRemove = a} :: UpdateTagsForResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resouce to be updated.
--
-- Must be the ARN of an Elastic Beanstalk resource.
updateTagsForResource_resourceArn :: Lens.Lens' UpdateTagsForResource Prelude.Text
updateTagsForResource_resourceArn = Lens.lens (\UpdateTagsForResource' {resourceArn} -> resourceArn) (\s@UpdateTagsForResource' {} a -> s {resourceArn = a} :: UpdateTagsForResource)

instance Core.AWSRequest UpdateTagsForResource where
  type
    AWSResponse UpdateTagsForResource =
      UpdateTagsForResourceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull UpdateTagsForResourceResponse'

instance Prelude.Hashable UpdateTagsForResource where
  hashWithSalt _salt UpdateTagsForResource' {..} =
    _salt
      `Prelude.hashWithSalt` tagsToAdd
      `Prelude.hashWithSalt` tagsToRemove
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData UpdateTagsForResource where
  rnf UpdateTagsForResource' {..} =
    Prelude.rnf tagsToAdd
      `Prelude.seq` Prelude.rnf tagsToRemove
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders UpdateTagsForResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateTagsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTagsForResource where
  toQuery UpdateTagsForResource' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateTagsForResource" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "TagsToAdd"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tagsToAdd),
        "TagsToRemove"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tagsToRemove),
        "ResourceArn" Data.=: resourceArn
      ]

-- | /See:/ 'newUpdateTagsForResourceResponse' smart constructor.
data UpdateTagsForResourceResponse = UpdateTagsForResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateTagsForResourceResponse ::
  UpdateTagsForResourceResponse
newUpdateTagsForResourceResponse =
  UpdateTagsForResourceResponse'

instance Prelude.NFData UpdateTagsForResourceResponse where
  rnf _ = ()
