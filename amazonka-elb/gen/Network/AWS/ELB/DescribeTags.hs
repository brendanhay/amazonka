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
-- Module      : Network.AWS.ELB.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
module Network.AWS.ELB.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_loadBalancerNames,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_tagDescriptions,
    describeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeTags.
--
-- /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The names of the load balancers.
    loadBalancerNames :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerNames', 'describeTags_loadBalancerNames' - The names of the load balancers.
newDescribeTags ::
  -- | 'loadBalancerNames'
  Core.NonEmpty Core.Text ->
  DescribeTags
newDescribeTags pLoadBalancerNames_ =
  DescribeTags'
    { loadBalancerNames =
        Lens._Coerce Lens.# pLoadBalancerNames_
    }

-- | The names of the load balancers.
describeTags_loadBalancerNames :: Lens.Lens' DescribeTags (Core.NonEmpty Core.Text)
describeTags_loadBalancerNames = Lens.lens (\DescribeTags' {loadBalancerNames} -> loadBalancerNames) (\s@DescribeTags' {} a -> s {loadBalancerNames = a} :: DescribeTags) Core.. Lens._Coerce

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Core.<$> ( x Core..@? "TagDescriptions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTags

instance Core.NFData DescribeTags

instance Core.ToHeaders DescribeTags where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTags where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeTags" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerNames"
          Core.=: Core.toQueryList "member" loadBalancerNames
      ]

-- | Contains the output for DescribeTags.
--
-- /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | Information about the tags.
    tagDescriptions :: Core.Maybe [TagDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagDescriptions', 'describeTagsResponse_tagDescriptions' - Information about the tags.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { tagDescriptions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags.
describeTagsResponse_tagDescriptions :: Lens.Lens' DescribeTagsResponse (Core.Maybe [TagDescription])
describeTagsResponse_tagDescriptions = Lens.lens (\DescribeTagsResponse' {tagDescriptions} -> tagDescriptions) (\s@DescribeTagsResponse' {} a -> s {tagDescriptions = a} :: DescribeTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Core.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Core.NFData DescribeTagsResponse
