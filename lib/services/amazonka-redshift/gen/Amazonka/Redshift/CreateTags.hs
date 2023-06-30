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
-- Module      : Amazonka.Redshift.CreateTags
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Redshift.CreateTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Prelude.. Lens.coerced

instance Core.AWSRequest CreateTags where
  type AWSResponse CreateTags = CreateTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull CreateTagsResponse'

instance Prelude.Hashable CreateTags where
  hashWithSalt _salt CreateTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateTags where
  rnf CreateTags' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTags where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTags where
  toQuery CreateTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ResourceName" Data.=: resourceName,
        "Tags" Data.=: Data.toQueryList "Tag" tags
      ]

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateTagsResponse ::
  CreateTagsResponse
newCreateTagsResponse = CreateTagsResponse'

instance Prelude.NFData CreateTagsResponse where
  rnf _ = ()
