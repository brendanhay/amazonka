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
-- Module      : Network.AWS.Discovery.CreateTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more tags for configuration items. Tags are metadata that
-- help you categorize IT assets. This API accepts a list of multiple
-- configuration items.
module Network.AWS.Discovery.CreateTags
  ( -- * Creating a Request
    CreateTags (..),
    newCreateTags,

    -- * Request Lenses
    createTags_configurationIds,
    createTags_tags,

    -- * Destructuring the Response
    CreateTagsResponse (..),
    newCreateTagsResponse,

    -- * Response Lenses
    createTagsResponse_httpStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | A list of configuration items that you want to tag.
    configurationIds :: [Prelude.Text],
    -- | Tags that you want to associate with one or more configuration items.
    -- Specify the tags that you want to create in a /key/-/value/ format. For
    -- example:
    --
    -- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationIds', 'createTags_configurationIds' - A list of configuration items that you want to tag.
--
-- 'tags', 'createTags_tags' - Tags that you want to associate with one or more configuration items.
-- Specify the tags that you want to create in a /key/-/value/ format. For
-- example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
newCreateTags ::
  CreateTags
newCreateTags =
  CreateTags'
    { configurationIds = Prelude.mempty,
      tags = Prelude.mempty
    }

-- | A list of configuration items that you want to tag.
createTags_configurationIds :: Lens.Lens' CreateTags [Prelude.Text]
createTags_configurationIds = Lens.lens (\CreateTags' {configurationIds} -> configurationIds) (\s@CreateTags' {} a -> s {configurationIds = a} :: CreateTags) Prelude.. Prelude._Coerce

-- | Tags that you want to associate with one or more configuration items.
-- Specify the tags that you want to create in a /key/-/value/ format. For
-- example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
createTags_tags :: Lens.Lens' CreateTags [Tag]
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTags

instance Prelude.NFData CreateTags

instance Prelude.ToHeaders CreateTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSPoseidonService_V2015_11_01.CreateTags" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configurationIds" Prelude..= configurationIds),
            Prelude.Just ("tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath CreateTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTagsResponse_httpStatus' - The response's http status code.
newCreateTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTagsResponse
newCreateTagsResponse pHttpStatus_ =
  CreateTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createTagsResponse_httpStatus :: Lens.Lens' CreateTagsResponse Prelude.Int
createTagsResponse_httpStatus = Lens.lens (\CreateTagsResponse' {httpStatus} -> httpStatus) (\s@CreateTagsResponse' {} a -> s {httpStatus = a} :: CreateTagsResponse)

instance Prelude.NFData CreateTagsResponse
