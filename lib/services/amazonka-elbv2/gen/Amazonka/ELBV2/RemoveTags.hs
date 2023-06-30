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
-- Module      : Amazonka.ELBV2.RemoveTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified Elastic Load Balancing
-- resources. You can remove the tags for one or more Application Load
-- Balancers, Network Load Balancers, Gateway Load Balancers, target
-- groups, listeners, or rules.
module Amazonka.ELBV2.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_resourceArns,
    removeTags_tagKeys,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,

    -- * Response Lenses
    removeTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArns :: [Prelude.Text],
    -- | The tag keys for the tags to remove.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArns', 'removeTags_resourceArns' - The Amazon Resource Name (ARN) of the resource.
--
-- 'tagKeys', 'removeTags_tagKeys' - The tag keys for the tags to remove.
newRemoveTags ::
  RemoveTags
newRemoveTags =
  RemoveTags'
    { resourceArns = Prelude.mempty,
      tagKeys = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource.
removeTags_resourceArns :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_resourceArns = Lens.lens (\RemoveTags' {resourceArns} -> resourceArns) (\s@RemoveTags' {} a -> s {resourceArns = a} :: RemoveTags) Prelude.. Lens.coerced

-- | The tag keys for the tags to remove.
removeTags_tagKeys :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_tagKeys = Lens.lens (\RemoveTags' {tagKeys} -> tagKeys) (\s@RemoveTags' {} a -> s {tagKeys = a} :: RemoveTags) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RemoveTagsResult"
      ( \s h x ->
          RemoveTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTags where
  hashWithSalt _salt RemoveTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTags where
  rnf RemoveTags' {..} =
    Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemoveTags where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTags where
  toQuery RemoveTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RemoveTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "ResourceArns"
          Data.=: Data.toQueryList "member" resourceArns,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsResponse_httpStatus' - The response's http status code.
newRemoveTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsResponse
newRemoveTagsResponse pHttpStatus_ =
  RemoveTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeTagsResponse_httpStatus :: Lens.Lens' RemoveTagsResponse Prelude.Int
removeTagsResponse_httpStatus = Lens.lens (\RemoveTagsResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsResponse' {} a -> s {httpStatus = a} :: RemoveTagsResponse)

instance Prelude.NFData RemoveTagsResponse where
  rnf RemoveTagsResponse' {..} = Prelude.rnf httpStatus
