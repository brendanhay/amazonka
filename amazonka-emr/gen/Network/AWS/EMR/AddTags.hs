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
-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html Tag Clusters>.
module Network.AWS.EMR.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_resourceId,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input identifies a cluster and a list of tags to attach.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The Amazon EMR resource identifier to which tags will be added. This
    -- value must be a cluster identifier.
    resourceId :: Prelude.Text,
    -- | A list of tags to associate with a cluster and propagate to EC2
    -- instances. Tags are user-defined key-value pairs that consist of a
    -- required key string with a maximum of 128 characters, and an optional
    -- value string with a maximum of 256 characters.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'addTags_resourceId' - The Amazon EMR resource identifier to which tags will be added. This
-- value must be a cluster identifier.
--
-- 'tags', 'addTags_tags' - A list of tags to associate with a cluster and propagate to EC2
-- instances. Tags are user-defined key-value pairs that consist of a
-- required key string with a maximum of 128 characters, and an optional
-- value string with a maximum of 256 characters.
newAddTags ::
  -- | 'resourceId'
  Prelude.Text ->
  AddTags
newAddTags pResourceId_ =
  AddTags'
    { resourceId = pResourceId_,
      tags = Prelude.mempty
    }

-- | The Amazon EMR resource identifier to which tags will be added. This
-- value must be a cluster identifier.
addTags_resourceId :: Lens.Lens' AddTags Prelude.Text
addTags_resourceId = Lens.lens (\AddTags' {resourceId} -> resourceId) (\s@AddTags' {} a -> s {resourceId = a} :: AddTags)

-- | A list of tags to associate with a cluster and propagate to EC2
-- instances. Tags are user-defined key-value pairs that consist of a
-- required key string with a maximum of 128 characters, and an optional
-- value string with a maximum of 256 characters.
addTags_tags :: Lens.Lens' AddTags [Tag]
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTags

instance Prelude.NFData AddTags

instance Prelude.ToHeaders AddTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("ElasticMapReduce.AddTags" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just ("Tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath AddTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddTags where
  toQuery = Prelude.const Prelude.mempty

-- | This output indicates the result of adding tags to a resource.
--
-- /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Prelude.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Prelude.NFData AddTagsResponse
