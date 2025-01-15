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
-- Module      : Amazonka.ELB.AddTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified load balancer. Each load
-- balancer can have a maximum of 10 tags.
--
-- Each tag consists of a key and an optional value. If a tag with the same
-- key is already associated with the load balancer, @AddTags@ updates its
-- value.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Amazonka.ELB.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_loadBalancerNames,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for AddTags.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The name of the load balancer. You can specify one load balancer only.
    loadBalancerNames :: [Prelude.Text],
    -- | The tags.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerNames', 'addTags_loadBalancerNames' - The name of the load balancer. You can specify one load balancer only.
--
-- 'tags', 'addTags_tags' - The tags.
newAddTags ::
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  AddTags
newAddTags pTags_ =
  AddTags'
    { loadBalancerNames = Prelude.mempty,
      tags = Lens.coerced Lens.# pTags_
    }

-- | The name of the load balancer. You can specify one load balancer only.
addTags_loadBalancerNames :: Lens.Lens' AddTags [Prelude.Text]
addTags_loadBalancerNames = Lens.lens (\AddTags' {loadBalancerNames} -> loadBalancerNames) (\s@AddTags' {} a -> s {loadBalancerNames = a} :: AddTags) Prelude.. Lens.coerced

-- | The tags.
addTags_tags :: Lens.Lens' AddTags (Prelude.NonEmpty Tag)
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Prelude.. Lens.coerced

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AddTagsResult"
      ( \s h x ->
          AddTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTags where
  hashWithSalt _salt AddTags' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerNames
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AddTags where
  rnf AddTags' {..} =
    Prelude.rnf loadBalancerNames `Prelude.seq`
      Prelude.rnf tags

instance Data.ToHeaders AddTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AddTags where
  toPath = Prelude.const "/"

instance Data.ToQuery AddTags where
  toQuery AddTags' {..} =
    Prelude.mconcat
      [ "Action" Data.=: ("AddTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerNames"
          Data.=: Data.toQueryList "member" loadBalancerNames,
        "Tags" Data.=: Data.toQueryList "member" tags
      ]

-- | Contains the output of AddTags.
--
-- /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Prelude.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Prelude.NFData AddTagsResponse where
  rnf AddTagsResponse' {..} = Prelude.rnf httpStatus
