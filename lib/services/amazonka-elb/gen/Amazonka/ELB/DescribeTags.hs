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
-- Module      : Amazonka.ELB.DescribeTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tags associated with the specified load balancers.
module Amazonka.ELB.DescribeTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeTags.
--
-- /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The names of the load balancers.
    loadBalancerNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  DescribeTags
newDescribeTags pLoadBalancerNames_ =
  DescribeTags'
    { loadBalancerNames =
        Lens.coerced Lens.# pLoadBalancerNames_
    }

-- | The names of the load balancers.
describeTags_loadBalancerNames :: Lens.Lens' DescribeTags (Prelude.NonEmpty Prelude.Text)
describeTags_loadBalancerNames = Lens.lens (\DescribeTags' {loadBalancerNames} -> loadBalancerNames) (\s@DescribeTags' {} a -> s {loadBalancerNames = a} :: DescribeTags) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> ( x
                            Data..@? "TagDescriptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags where
  hashWithSalt _salt DescribeTags' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerNames

instance Prelude.NFData DescribeTags where
  rnf DescribeTags' {..} = Prelude.rnf loadBalancerNames

instance Data.ToHeaders DescribeTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerNames"
          Data.=: Data.toQueryList "member" loadBalancerNames
      ]

-- | Contains the output for DescribeTags.
--
-- /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | Information about the tags.
    tagDescriptions :: Prelude.Maybe [TagDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { tagDescriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags.
describeTagsResponse_tagDescriptions :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [TagDescription])
describeTagsResponse_tagDescriptions = Lens.lens (\DescribeTagsResponse' {tagDescriptions} -> tagDescriptions) (\s@DescribeTagsResponse' {} a -> s {tagDescriptions = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse where
  rnf DescribeTagsResponse' {..} =
    Prelude.rnf tagDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
