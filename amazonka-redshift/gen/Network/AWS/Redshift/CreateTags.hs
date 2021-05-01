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
-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a cluster.
--
-- A resource can have up to 50 tags. If you try to create more than 50
-- tags for a resource, you will receive an error and the attempt will
-- fail.
--
-- If you specify a key that already exists for the resource, the value for
-- that key will be updated with the new value.
module Network.AWS.Redshift.CreateTags
  ( -- * Creating a Request
    CreateTags (..),
    newCreateTags,

    -- * Request Lenses
    createTags_resourceName,
    createTags_tags,

    -- * Destructuring the Response
    CreateTagsResponse (..),
    newCreateTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the output from the @CreateTags@ action.
--
-- /See:/ 'newCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | The Amazon Resource Name (ARN) to which you want to add the tag or tags.
    -- For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
    resourceName :: Prelude.Text,
    -- | One or more name\/value pairs to add as tags to the specified resource.
    -- Each tag name is passed in with the parameter @Key@ and the
    -- corresponding value is passed in with the parameter @Value@. The @Key@
    -- and @Value@ parameters are separated by a comma (,). Separate multiple
    -- tags with a space. For example,
    -- @--tags \"Key\"=\"owner\",\"Value\"=\"admin\" \"Key\"=\"environment\",\"Value\"=\"test\" \"Key\"=\"version\",\"Value\"=\"1.0\"@.
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
-- 'resourceName', 'createTags_resourceName' - The Amazon Resource Name (ARN) to which you want to add the tag or tags.
-- For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
--
-- 'tags', 'createTags_tags' - One or more name\/value pairs to add as tags to the specified resource.
-- Each tag name is passed in with the parameter @Key@ and the
-- corresponding value is passed in with the parameter @Value@. The @Key@
-- and @Value@ parameters are separated by a comma (,). Separate multiple
-- tags with a space. For example,
-- @--tags \"Key\"=\"owner\",\"Value\"=\"admin\" \"Key\"=\"environment\",\"Value\"=\"test\" \"Key\"=\"version\",\"Value\"=\"1.0\"@.
newCreateTags ::
  -- | 'resourceName'
  Prelude.Text ->
  CreateTags
newCreateTags pResourceName_ =
  CreateTags'
    { resourceName = pResourceName_,
      tags = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags.
-- For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
createTags_resourceName :: Lens.Lens' CreateTags Prelude.Text
createTags_resourceName = Lens.lens (\CreateTags' {resourceName} -> resourceName) (\s@CreateTags' {} a -> s {resourceName = a} :: CreateTags)

-- | One or more name\/value pairs to add as tags to the specified resource.
-- Each tag name is passed in with the parameter @Key@ and the
-- corresponding value is passed in with the parameter @Value@. The @Key@
-- and @Value@ parameters are separated by a comma (,). Separate multiple
-- tags with a space. For example,
-- @--tags \"Key\"=\"owner\",\"Value\"=\"admin\" \"Key\"=\"environment\",\"Value\"=\"test\" \"Key\"=\"version\",\"Value\"=\"1.0\"@.
createTags_tags :: Lens.Lens' CreateTags [Tag]
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull CreateTagsResponse'

instance Prelude.Hashable CreateTags

instance Prelude.NFData CreateTags

instance Prelude.ToHeaders CreateTags where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTags where
  toQuery CreateTags' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateTags" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ResourceName" Prelude.=: resourceName,
        "Tags" Prelude.=: Prelude.toQueryList "Tag" tags
      ]

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateTagsResponse ::
  CreateTagsResponse
newCreateTagsResponse = CreateTagsResponse'

instance Prelude.NFData CreateTagsResponse
