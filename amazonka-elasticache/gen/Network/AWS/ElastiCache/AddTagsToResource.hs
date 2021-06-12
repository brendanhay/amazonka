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
-- Module      : Network.AWS.ElastiCache.AddTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds up to 50 cost allocation tags to the named resource. A cost
-- allocation tag is a key-value pair where the key and value are
-- case-sensitive. You can use cost allocation tags to categorize and track
-- your AWS costs.
--
-- When you apply tags to your ElastiCache resources, AWS generates a cost
-- allocation report as a comma-separated value (CSV) file with your usage
-- and costs aggregated by your tags. You can apply tags that represent
-- business categories (such as cost centers, application names, or owners)
-- to organize your costs across multiple services. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Tagging.html Using Cost Allocation Tags in Amazon ElastiCache>
-- in the /ElastiCache User Guide/.
module Network.AWS.ElastiCache.AddTagsToResource
  ( -- * Creating a Request
    AddTagsToResource (..),
    newAddTagsToResource,

    -- * Request Lenses
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- * Destructuring the Response
    TagListMessage (..),
    newTagListMessage,

    -- * Response Lenses
    tagListMessage_tagList,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AddTagsToResource operation.
--
-- /See:/ 'newAddTagsToResource' smart constructor.
data AddTagsToResource = AddTagsToResource'
  { -- | The Amazon Resource Name (ARN) of the resource to which the tags are to
    -- be added, for example
    -- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
    -- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
    -- ElastiCache resources are /cluster/ and /snapshot/.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    resourceName :: Core.Text,
    -- | A list of cost allocation tags to be added to this resource. A tag is a
    -- key-value pair. A tag key must be accompanied by a tag value.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'addTagsToResource_resourceName' - The Amazon Resource Name (ARN) of the resource to which the tags are to
-- be added, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
-- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
-- ElastiCache resources are /cluster/ and /snapshot/.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
--
-- 'tags', 'addTagsToResource_tags' - A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
newAddTagsToResource ::
  -- | 'resourceName'
  Core.Text ->
  AddTagsToResource
newAddTagsToResource pResourceName_ =
  AddTagsToResource'
    { resourceName = pResourceName_,
      tags = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource to which the tags are to
-- be added, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
-- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
-- ElastiCache resources are /cluster/ and /snapshot/.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
addTagsToResource_resourceName :: Lens.Lens' AddTagsToResource Core.Text
addTagsToResource_resourceName = Lens.lens (\AddTagsToResource' {resourceName} -> resourceName) (\s@AddTagsToResource' {} a -> s {resourceName = a} :: AddTagsToResource)

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
addTagsToResource_tags :: Lens.Lens' AddTagsToResource [Tag]
addTagsToResource_tags = Lens.lens (\AddTagsToResource' {tags} -> tags) (\s@AddTagsToResource' {} a -> s {tags = a} :: AddTagsToResource) Core.. Lens._Coerce

instance Core.AWSRequest AddTagsToResource where
  type AWSResponse AddTagsToResource = TagListMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AddTagsToResourceResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable AddTagsToResource

instance Core.NFData AddTagsToResource

instance Core.ToHeaders AddTagsToResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddTagsToResource where
  toPath = Core.const "/"

instance Core.ToQuery AddTagsToResource where
  toQuery AddTagsToResource' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AddTagsToResource" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "ResourceName" Core.=: resourceName,
        "Tags" Core.=: Core.toQueryList "Tag" tags
      ]
