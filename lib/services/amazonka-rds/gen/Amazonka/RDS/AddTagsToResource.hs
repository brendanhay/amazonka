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
-- Module      : Amazonka.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds metadata tags to an Amazon RDS resource. These tags can also be
-- used with cost allocation reporting to track cost associated with Amazon
-- RDS resources, or used in a Condition statement in an IAM policy for
-- Amazon RDS.
--
-- For an overview on tagging Amazon RDS resources, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources>.
module Amazonka.RDS.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- * Destructuring the Response
    AddTagsToResourceResponse (..),
    newAddTagsToResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon RDS resource that the tags are added to. This value is an
    -- Amazon Resource Name (ARN). For information about creating an ARN, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
    resourceName :: Prelude.Text,
    -- | The tags to be assigned to the Amazon RDS resource.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'addTagsToResource_resourceName' - The Amazon RDS resource that the tags are added to. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
--
-- 'tags', 'addTagsToResource_tags' - The tags to be assigned to the Amazon RDS resource.
newAddTagsToResource ::
  -- | 'resourceName'
  Prelude.Text ->
  AddTagsToResource
newAddTagsToResource pResourceName_ =
  AddTagsToResource'
    { resourceName = pResourceName_,
      tags = Prelude.mempty
    }

-- | The Amazon RDS resource that the tags are added to. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)>.
addTagsToResource_resourceName :: Lens.Lens' AddTagsToResource Prelude.Text
addTagsToResource_resourceName = Lens.lens (\AddTagsToResource' {resourceName} -> resourceName) (\s@AddTagsToResource' {} a -> s {resourceName = a} :: AddTagsToResource)

-- | The tags to be assigned to the Amazon RDS resource.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Prelude.. Lens.coerced

instance Core.AWSRequest AddTagsToResource where
  type
    AWSResponse AddTagsToResource =
      AddTagsToResourceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AddTagsToResourceResponse'

instance Prelude.Hashable AddTagsToResource where
  hashWithSalt _salt AddTagsToResource' {..} =
    _salt `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AddTagsToResource where
  rnf AddTagsToResource' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders AddTagsToResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AddTagsToResource where
  toPath = Prelude.const "/"

instance Data.ToQuery AddTagsToResource where
  toQuery AddTagsToResource' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AddTagsToResource" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ResourceName" Data.=: resourceName,
        "Tags" Data.=: Data.toQueryList "Tag" tags
      ]

-- | /See:/ 'newAddTagsToResourceResponse' smart constructor.
data AddTagsToResourceResponse = AddTagsToResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToResourceResponse ::
  AddTagsToResourceResponse
newAddTagsToResourceResponse =
  AddTagsToResourceResponse'

instance Prelude.NFData AddTagsToResourceResponse where
  rnf _ = ()
