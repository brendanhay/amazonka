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
-- Module      : Amazonka.ElasticBeanstalk.ListTagsForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ElasticBeanstalk.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceArn,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_resourceTags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> ( x Data..@? "ResourceTags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ResourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource where
  hashWithSalt _salt ListTagsForResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListTagsForResource where
  rnf ListTagsForResource' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders ListTagsForResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListTagsForResource" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ResourceArn" Data.=: resourceArn
      ]

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A list of tag key-value pairs.
    resourceTags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the resource for which a tag list was
    -- requested.
    resourceArn :: Prelude.Maybe Prelude.Text,
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
-- 'resourceTags', 'listTagsForResourceResponse_resourceTags' - A list of tag key-value pairs.
--
-- 'resourceArn', 'listTagsForResourceResponse_resourceArn' - The Amazon Resource Name (ARN) of the resource for which a tag list was
-- requested.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { resourceTags =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tag key-value pairs.
listTagsForResourceResponse_resourceTags :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe [Tag])
listTagsForResourceResponse_resourceTags = Lens.lens (\ListTagsForResourceResponse' {resourceTags} -> resourceTags) (\s@ListTagsForResourceResponse' {} a -> s {resourceTags = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource for which a tag list was
-- requested.
listTagsForResourceResponse_resourceArn :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_resourceArn = Lens.lens (\ListTagsForResourceResponse' {resourceArn} -> resourceArn) (\s@ListTagsForResourceResponse' {} a -> s {resourceArn = a} :: ListTagsForResourceResponse)

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse where
  rnf ListTagsForResourceResponse' {..} =
    Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus
