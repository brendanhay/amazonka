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
-- Module      : Network.AWS.ElasticBeanstalk.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return the tags applied to an AWS Elastic Beanstalk resource. The
-- response contains a list of tag key-value pairs.
--
-- Elastic Beanstalk supports tagging of all of its resources. For details
-- about resource tagging, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/applications-tagging-resources.html Tagging Application Resources>.
module Network.AWS.ElasticBeanstalk.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceArn,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The Amazon Resource Name (ARN) of the resouce for which a tag list is
    -- requested.
    --
    -- Must be the ARN of an Elastic Beanstalk resource.
    resourceArn :: Prelude.Text
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
-- 'resourceArn', 'listTagsForResource_resourceArn' - The Amazon Resource Name (ARN) of the resouce for which a tag list is
-- requested.
--
-- Must be the ARN of an Elastic Beanstalk resource.
newListTagsForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceArn_ =
  ListTagsForResource' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the resouce for which a tag list is
-- requested.
--
-- Must be the ARN of an Elastic Beanstalk resource.
listTagsForResource_resourceArn :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceArn = Lens.lens (\ListTagsForResource' {resourceArn} -> resourceArn) (\s@ListTagsForResource' {} a -> s {resourceArn = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (x Core..@? "ResourceArn")
            Prelude.<*> ( x Core..@? "ResourceTags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

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
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ResourceArn" Core.=: resourceArn
      ]

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource for which a tag list was
    -- requested.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | A list of tag key-value pairs.
    resourceTags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTagsForResourceResponse_resourceArn' - The Amazon Resource Name (ARN) of the resource for which a tag list was
-- requested.
--
-- 'resourceTags', 'listTagsForResourceResponse_resourceTags' - A list of tag key-value pairs.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { resourceArn =
        Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource for which a tag list was
-- requested.
listTagsForResourceResponse_resourceArn :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_resourceArn = Lens.lens (\ListTagsForResourceResponse' {resourceArn} -> resourceArn) (\s@ListTagsForResourceResponse' {} a -> s {resourceArn = a} :: ListTagsForResourceResponse)

-- | A list of tag key-value pairs.
listTagsForResourceResponse_resourceTags :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe [Tag])
listTagsForResourceResponse_resourceTags = Lens.lens (\ListTagsForResourceResponse' {resourceTags} -> resourceTags) (\s@ListTagsForResourceResponse' {} a -> s {resourceTags = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse
