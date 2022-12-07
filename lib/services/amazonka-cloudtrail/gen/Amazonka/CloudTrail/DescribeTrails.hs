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
-- Module      : Amazonka.CloudTrail.DescribeTrails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves settings for one or more trails associated with the current
-- region for your account.
module Amazonka.CloudTrail.DescribeTrails
  ( -- * Creating a Request
    DescribeTrails (..),
    newDescribeTrails,

    -- * Request Lenses
    describeTrails_trailNameList,
    describeTrails_includeShadowTrails,

    -- * Destructuring the Response
    DescribeTrailsResponse (..),
    newDescribeTrailsResponse,

    -- * Response Lenses
    describeTrailsResponse_trailList,
    describeTrailsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Returns information about the trail.
--
-- /See:/ 'newDescribeTrails' smart constructor.
data DescribeTrails = DescribeTrails'
  { -- | Specifies a list of trail names, trail ARNs, or both, of the trails to
    -- describe. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    --
    -- If an empty list is specified, information for the trail in the current
    -- region is returned.
    --
    -- -   If an empty list is specified and @IncludeShadowTrails@ is false,
    --     then information for all trails in the current region is returned.
    --
    -- -   If an empty list is specified and IncludeShadowTrails is null or
    --     true, then information for all trails in the current region and any
    --     associated shadow trails in other regions is returned.
    --
    -- If one or more trail names are specified, information is returned only
    -- if the names match the names of trails belonging only to the current
    -- region. To return information about a trail in another region, you must
    -- specify its trail ARN.
    trailNameList :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether to include shadow trails in the response. A shadow
    -- trail is the replication in a region of a trail that was created in a
    -- different region, or in the case of an organization trail, the
    -- replication of an organization trail in member accounts. If you do not
    -- include shadow trails, organization trails in a member account and
    -- region replication trails will not be returned. The default is true.
    includeShadowTrails :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailNameList', 'describeTrails_trailNameList' - Specifies a list of trail names, trail ARNs, or both, of the trails to
-- describe. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- If an empty list is specified, information for the trail in the current
-- region is returned.
--
-- -   If an empty list is specified and @IncludeShadowTrails@ is false,
--     then information for all trails in the current region is returned.
--
-- -   If an empty list is specified and IncludeShadowTrails is null or
--     true, then information for all trails in the current region and any
--     associated shadow trails in other regions is returned.
--
-- If one or more trail names are specified, information is returned only
-- if the names match the names of trails belonging only to the current
-- region. To return information about a trail in another region, you must
-- specify its trail ARN.
--
-- 'includeShadowTrails', 'describeTrails_includeShadowTrails' - Specifies whether to include shadow trails in the response. A shadow
-- trail is the replication in a region of a trail that was created in a
-- different region, or in the case of an organization trail, the
-- replication of an organization trail in member accounts. If you do not
-- include shadow trails, organization trails in a member account and
-- region replication trails will not be returned. The default is true.
newDescribeTrails ::
  DescribeTrails
newDescribeTrails =
  DescribeTrails'
    { trailNameList = Prelude.Nothing,
      includeShadowTrails = Prelude.Nothing
    }

-- | Specifies a list of trail names, trail ARNs, or both, of the trails to
-- describe. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- If an empty list is specified, information for the trail in the current
-- region is returned.
--
-- -   If an empty list is specified and @IncludeShadowTrails@ is false,
--     then information for all trails in the current region is returned.
--
-- -   If an empty list is specified and IncludeShadowTrails is null or
--     true, then information for all trails in the current region and any
--     associated shadow trails in other regions is returned.
--
-- If one or more trail names are specified, information is returned only
-- if the names match the names of trails belonging only to the current
-- region. To return information about a trail in another region, you must
-- specify its trail ARN.
describeTrails_trailNameList :: Lens.Lens' DescribeTrails (Prelude.Maybe [Prelude.Text])
describeTrails_trailNameList = Lens.lens (\DescribeTrails' {trailNameList} -> trailNameList) (\s@DescribeTrails' {} a -> s {trailNameList = a} :: DescribeTrails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to include shadow trails in the response. A shadow
-- trail is the replication in a region of a trail that was created in a
-- different region, or in the case of an organization trail, the
-- replication of an organization trail in member accounts. If you do not
-- include shadow trails, organization trails in a member account and
-- region replication trails will not be returned. The default is true.
describeTrails_includeShadowTrails :: Lens.Lens' DescribeTrails (Prelude.Maybe Prelude.Bool)
describeTrails_includeShadowTrails = Lens.lens (\DescribeTrails' {includeShadowTrails} -> includeShadowTrails) (\s@DescribeTrails' {} a -> s {includeShadowTrails = a} :: DescribeTrails)

instance Core.AWSRequest DescribeTrails where
  type
    AWSResponse DescribeTrails =
      DescribeTrailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrailsResponse'
            Prelude.<$> (x Data..?> "trailList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrails where
  hashWithSalt _salt DescribeTrails' {..} =
    _salt `Prelude.hashWithSalt` trailNameList
      `Prelude.hashWithSalt` includeShadowTrails

instance Prelude.NFData DescribeTrails where
  rnf DescribeTrails' {..} =
    Prelude.rnf trailNameList
      `Prelude.seq` Prelude.rnf includeShadowTrails

instance Data.ToHeaders DescribeTrails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DescribeTrails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTrails where
  toJSON DescribeTrails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("trailNameList" Data..=) Prelude.<$> trailNameList,
            ("includeShadowTrails" Data..=)
              Prelude.<$> includeShadowTrails
          ]
      )

instance Data.ToPath DescribeTrails where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrails where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newDescribeTrailsResponse' smart constructor.
data DescribeTrailsResponse = DescribeTrailsResponse'
  { -- | The list of trail objects. Trail objects with string values are only
    -- returned if values for the objects exist in a trail\'s configuration.
    -- For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in
    -- results if a trail is configured to send SNS notifications. Similarly,
    -- @KMSKeyId@ only appears in results if a trail\'s log files are encrypted
    -- with KMS customer managed keys.
    trailList :: Prelude.Maybe [Trail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailList', 'describeTrailsResponse_trailList' - The list of trail objects. Trail objects with string values are only
-- returned if values for the objects exist in a trail\'s configuration.
-- For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in
-- results if a trail is configured to send SNS notifications. Similarly,
-- @KMSKeyId@ only appears in results if a trail\'s log files are encrypted
-- with KMS customer managed keys.
--
-- 'httpStatus', 'describeTrailsResponse_httpStatus' - The response's http status code.
newDescribeTrailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrailsResponse
newDescribeTrailsResponse pHttpStatus_ =
  DescribeTrailsResponse'
    { trailList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of trail objects. Trail objects with string values are only
-- returned if values for the objects exist in a trail\'s configuration.
-- For example, @SNSTopicName@ and @SNSTopicARN@ are only returned in
-- results if a trail is configured to send SNS notifications. Similarly,
-- @KMSKeyId@ only appears in results if a trail\'s log files are encrypted
-- with KMS customer managed keys.
describeTrailsResponse_trailList :: Lens.Lens' DescribeTrailsResponse (Prelude.Maybe [Trail])
describeTrailsResponse_trailList = Lens.lens (\DescribeTrailsResponse' {trailList} -> trailList) (\s@DescribeTrailsResponse' {} a -> s {trailList = a} :: DescribeTrailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrailsResponse_httpStatus :: Lens.Lens' DescribeTrailsResponse Prelude.Int
describeTrailsResponse_httpStatus = Lens.lens (\DescribeTrailsResponse' {httpStatus} -> httpStatus) (\s@DescribeTrailsResponse' {} a -> s {httpStatus = a} :: DescribeTrailsResponse)

instance Prelude.NFData DescribeTrailsResponse where
  rnf DescribeTrailsResponse' {..} =
    Prelude.rnf trailList
      `Prelude.seq` Prelude.rnf httpStatus
