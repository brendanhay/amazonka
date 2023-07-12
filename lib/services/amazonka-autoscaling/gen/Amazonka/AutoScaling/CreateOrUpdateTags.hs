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
-- Module      : Amazonka.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates tags for the specified Auto Scaling group.
--
-- When you specify a tag with a key that already exists, the operation
-- overwrites the previous tag definition, and you do not get an error
-- message.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-tagging.html Tag Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.CreateOrUpdateTags
  ( -- * Creating a Request
    CreateOrUpdateTags (..),
    newCreateOrUpdateTags,

    -- * Request Lenses
    createOrUpdateTags_tags,

    -- * Destructuring the Response
    CreateOrUpdateTagsResponse (..),
    newCreateOrUpdateTagsResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOrUpdateTags' smart constructor.
data CreateOrUpdateTags = CreateOrUpdateTags'
  { -- | One or more tags.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrUpdateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOrUpdateTags_tags' - One or more tags.
newCreateOrUpdateTags ::
  CreateOrUpdateTags
newCreateOrUpdateTags =
  CreateOrUpdateTags' {tags = Prelude.mempty}

-- | One or more tags.
createOrUpdateTags_tags :: Lens.Lens' CreateOrUpdateTags [Tag]
createOrUpdateTags_tags = Lens.lens (\CreateOrUpdateTags' {tags} -> tags) (\s@CreateOrUpdateTags' {} a -> s {tags = a} :: CreateOrUpdateTags) Prelude.. Lens.coerced

instance Core.AWSRequest CreateOrUpdateTags where
  type
    AWSResponse CreateOrUpdateTags =
      CreateOrUpdateTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull CreateOrUpdateTagsResponse'

instance Prelude.Hashable CreateOrUpdateTags where
  hashWithSalt _salt CreateOrUpdateTags' {..} =
    _salt `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateOrUpdateTags where
  rnf CreateOrUpdateTags' {..} = Prelude.rnf tags

instance Data.ToHeaders CreateOrUpdateTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateOrUpdateTags where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateOrUpdateTags where
  toQuery CreateOrUpdateTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateOrUpdateTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "Tags" Data.=: Data.toQueryList "member" tags
      ]

-- | /See:/ 'newCreateOrUpdateTagsResponse' smart constructor.
data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrUpdateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateOrUpdateTagsResponse ::
  CreateOrUpdateTagsResponse
newCreateOrUpdateTagsResponse =
  CreateOrUpdateTagsResponse'

instance Prelude.NFData CreateOrUpdateTagsResponse where
  rnf _ = ()
