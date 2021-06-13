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
-- Module      : Network.AWS.ElastiCache.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all cost allocation tags currently on the named resource. A
-- @cost allocation tag@ is a key-value pair where the key is
-- case-sensitive and the value is optional. You can use cost allocation
-- tags to categorize and track your AWS costs.
--
-- If the cluster is not in the /available/ state, @ListTagsForResource@
-- returns an error.
--
-- You can have a maximum of 50 cost allocation tags on an ElastiCache
-- resource. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Tagging.html Monitoring Costs with Tags>.
module Network.AWS.ElastiCache.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceName,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input parameters for the @ListTagsForResource@ operation.
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The Amazon Resource Name (ARN) of the resource for which you want the
    -- list of tags, for example
    -- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
    -- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'listTagsForResource_resourceName' - The Amazon Resource Name (ARN) of the resource for which you want the
-- list of tags, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
-- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
newListTagsForResource ::
  -- | 'resourceName'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceName_ =
  ListTagsForResource' {resourceName = pResourceName_}

-- | The Amazon Resource Name (ARN) of the resource for which you want the
-- list of tags, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@ or
-- @arn:aws:elasticache:us-west-2:0123456789:snapshot:mySnapshot@.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
listTagsForResource_resourceName :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceName = Lens.lens (\ListTagsForResource' {resourceName} -> resourceName) (\s@ListTagsForResource' {} a -> s {resourceName = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type AWSResponse ListTagsForResource = TagListMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTagsForResourceResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ListTagsForResource

instance Prelude.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListTagsForResource" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ResourceName" Core.=: resourceName
      ]
