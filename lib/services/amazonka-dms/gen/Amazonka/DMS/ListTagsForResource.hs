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
-- Module      : Amazonka.DMS.ListTagsForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all metadata tags attached to an DMS resource, including
-- replication instance, endpoint, subnet group, and migration task. For
-- more information, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_Tag.html Tag>
-- data type description.
module Amazonka.DMS.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceArn,
    listTagsForResource_resourceArnList,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the DMS
    -- resource to list tags for. This returns a list of keys (names of tags)
    -- created for the resource and their associated tag values.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | List of ARNs that identify multiple DMS resources that you want to list
    -- tags for. This returns a list of keys (tag names) and their associated
    -- tag values. It also returns each tag\'s associated @ResourceArn@ value,
    -- which is the ARN of the resource for which each listed tag is created.
    resourceArnList :: Prelude.Maybe [Prelude.Text]
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
-- 'resourceArn', 'listTagsForResource_resourceArn' - The Amazon Resource Name (ARN) string that uniquely identifies the DMS
-- resource to list tags for. This returns a list of keys (names of tags)
-- created for the resource and their associated tag values.
--
-- 'resourceArnList', 'listTagsForResource_resourceArnList' - List of ARNs that identify multiple DMS resources that you want to list
-- tags for. This returns a list of keys (tag names) and their associated
-- tag values. It also returns each tag\'s associated @ResourceArn@ value,
-- which is the ARN of the resource for which each listed tag is created.
newListTagsForResource ::
  ListTagsForResource
newListTagsForResource =
  ListTagsForResource'
    { resourceArn = Prelude.Nothing,
      resourceArnList = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the DMS
-- resource to list tags for. This returns a list of keys (names of tags)
-- created for the resource and their associated tag values.
listTagsForResource_resourceArn :: Lens.Lens' ListTagsForResource (Prelude.Maybe Prelude.Text)
listTagsForResource_resourceArn = Lens.lens (\ListTagsForResource' {resourceArn} -> resourceArn) (\s@ListTagsForResource' {} a -> s {resourceArn = a} :: ListTagsForResource)

-- | List of ARNs that identify multiple DMS resources that you want to list
-- tags for. This returns a list of keys (tag names) and their associated
-- tag values. It also returns each tag\'s associated @ResourceArn@ value,
-- which is the ARN of the resource for which each listed tag is created.
listTagsForResource_resourceArnList :: Lens.Lens' ListTagsForResource (Prelude.Maybe [Prelude.Text])
listTagsForResource_resourceArnList = Lens.lens (\ListTagsForResource' {resourceArnList} -> resourceArnList) (\s@ListTagsForResource' {} a -> s {resourceArnList = a} :: ListTagsForResource) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (x Data..?> "TagList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource where
  hashWithSalt _salt ListTagsForResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceArnList

instance Prelude.NFData ListTagsForResource where
  rnf ListTagsForResource' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceArnList

instance Data.ToHeaders ListTagsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.ListTagsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceArn" Data..=) Prelude.<$> resourceArn,
            ("ResourceArnList" Data..=)
              Prelude.<$> resourceArnList
          ]
      )

instance Data.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A list of tags for the resource.
    tagList :: Prelude.Maybe [Tag],
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
-- 'tagList', 'listTagsForResourceResponse_tagList' - A list of tags for the resource.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { tagList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tags for the resource.
listTagsForResourceResponse_tagList :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe [Tag])
listTagsForResourceResponse_tagList = Lens.lens (\ListTagsForResourceResponse' {tagList} -> tagList) (\s@ListTagsForResourceResponse' {} a -> s {tagList = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse where
  rnf ListTagsForResourceResponse' {..} =
    Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf httpStatus
