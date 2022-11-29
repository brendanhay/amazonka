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
-- Module      : Amazonka.ElastiCache.RemoveTagsFromResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the tags identified by the @TagKeys@ list from the named
-- resource. A tag is a key-value pair where the key and value are
-- case-sensitive. You can use tags to categorize and track all your
-- ElastiCache resources, with the exception of global replication group.
-- When you add or remove tags on replication groups, those actions will be
-- replicated to all nodes in the replication group. For more information,
-- see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/IAM.ResourceLevelPermissions.html Resource-level permissions>.
module Amazonka.ElastiCache.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- * Destructuring the Response
    TagListMessage (..),
    newTagListMessage,

    -- * Response Lenses
    tagListMessage_tagList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @RemoveTagsFromResource@ operation.
--
-- /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The Amazon Resource Name (ARN) of the resource from which you want the
    -- tags removed, for example
    -- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
    -- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Service Namespaces>.
    resourceName :: Prelude.Text,
    -- | A list of @TagKeys@ identifying the tags you want removed from the named
    -- resource.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'removeTagsFromResource_resourceName' - The Amazon Resource Name (ARN) of the resource from which you want the
-- tags removed, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
-- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Service Namespaces>.
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - A list of @TagKeys@ identifying the tags you want removed from the named
-- resource.
newRemoveTagsFromResource ::
  -- | 'resourceName'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceName_ =
  RemoveTagsFromResource'
    { resourceName =
        pResourceName_,
      tagKeys = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource from which you want the
-- tags removed, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
-- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Service Namespaces>.
removeTagsFromResource_resourceName :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceName = Lens.lens (\RemoveTagsFromResource' {resourceName} -> resourceName) (\s@RemoveTagsFromResource' {} a -> s {resourceName = a} :: RemoveTagsFromResource)

-- | A list of @TagKeys@ identifying the tags you want removed from the named
-- resource.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromResource where
  type
    AWSResponse RemoveTagsFromResource =
      TagListMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RemoveTagsFromResourceResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable RemoveTagsFromResource where
  hashWithSalt _salt RemoveTagsFromResource' {..} =
    _salt `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromResource where
  rnf RemoveTagsFromResource' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf tagKeys

instance Core.ToHeaders RemoveTagsFromResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTagsFromResource where
  toQuery RemoveTagsFromResource' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RemoveTagsFromResource" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ResourceName" Core.=: resourceName,
        "TagKeys" Core.=: Core.toQueryList "member" tagKeys
      ]
