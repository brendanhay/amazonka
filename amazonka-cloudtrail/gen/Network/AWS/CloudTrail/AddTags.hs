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
-- Module      : Network.AWS.CloudTrail.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a trail, up to a limit of 50. Overwrites an
-- existing tag\'s value when a new value is specified for an existing tag
-- key. Tag key names must be unique for a trail; you cannot have two keys
-- with the same name but different values. If you specify a key without a
-- value, the tag will be created with the specified key and a value of
-- null. You can tag a trail that applies to all AWS Regions only from the
-- Region in which the trail was created (also known as its home region).
module Network.AWS.CloudTrail.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_tagsList,
    addTags_resourceId,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies the tags to add to a trail.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | Contains a list of CloudTrail tags, up to a limit of 50
    tagsList :: Prelude.Maybe [Tag],
    -- | Specifies the ARN of the trail to which one or more tags will be added.
    -- The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    resourceId :: Prelude.Text
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
-- 'tagsList', 'addTags_tagsList' - Contains a list of CloudTrail tags, up to a limit of 50
--
-- 'resourceId', 'addTags_resourceId' - Specifies the ARN of the trail to which one or more tags will be added.
-- The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newAddTags ::
  -- | 'resourceId'
  Prelude.Text ->
  AddTags
newAddTags pResourceId_ =
  AddTags'
    { tagsList = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | Contains a list of CloudTrail tags, up to a limit of 50
addTags_tagsList :: Lens.Lens' AddTags (Prelude.Maybe [Tag])
addTags_tagsList = Lens.lens (\AddTags' {tagsList} -> tagsList) (\s@AddTags' {} a -> s {tagsList = a} :: AddTags) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the ARN of the trail to which one or more tags will be added.
-- The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
addTags_resourceId :: Lens.Lens' AddTags Prelude.Text
addTags_resourceId = Lens.lens (\AddTags' {resourceId} -> resourceId) (\s@AddTags' {} a -> s {resourceId = a} :: AddTags)

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
              Prelude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.AddTags" ::
                             Prelude.ByteString
                         ),
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
          [ ("TagsList" Prelude..=) Prelude.<$> tagsList,
            Prelude.Just ("ResourceId" Prelude..= resourceId)
          ]
      )

instance Prelude.ToPath AddTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddTags where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
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
