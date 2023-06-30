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
-- Module      : Amazonka.AutoScaling.DetachTrafficSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Reserved for use with Amazon VPC Lattice, which is in preview and
-- subject to change. Do not use this API for production workloads. This
-- API is also subject to change.__
--
-- Detaches one or more traffic sources from the specified Auto Scaling
-- group.
module Amazonka.AutoScaling.DetachTrafficSources
  ( -- * Creating a Request
    DetachTrafficSources (..),
    newDetachTrafficSources,

    -- * Request Lenses
    detachTrafficSources_autoScalingGroupName,
    detachTrafficSources_trafficSources,

    -- * Destructuring the Response
    DetachTrafficSourcesResponse (..),
    newDetachTrafficSourcesResponse,

    -- * Response Lenses
    detachTrafficSourcesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachTrafficSources' smart constructor.
data DetachTrafficSources = DetachTrafficSources'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The unique identifiers of one or more traffic sources you are detaching.
    -- You can specify up to 10 traffic sources.
    --
    -- Currently, you must specify an Amazon Resource Name (ARN) for an
    -- existing VPC Lattice target group. When you detach a target group, it
    -- enters the @Removing@ state while deregistering the instances in the
    -- group. When all instances are deregistered, then you can no longer
    -- describe the target group using the DescribeTrafficSources API call. The
    -- instances continue to run.
    trafficSources :: [TrafficSourceIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachTrafficSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'detachTrafficSources_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'trafficSources', 'detachTrafficSources_trafficSources' - The unique identifiers of one or more traffic sources you are detaching.
-- You can specify up to 10 traffic sources.
--
-- Currently, you must specify an Amazon Resource Name (ARN) for an
-- existing VPC Lattice target group. When you detach a target group, it
-- enters the @Removing@ state while deregistering the instances in the
-- group. When all instances are deregistered, then you can no longer
-- describe the target group using the DescribeTrafficSources API call. The
-- instances continue to run.
newDetachTrafficSources ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DetachTrafficSources
newDetachTrafficSources pAutoScalingGroupName_ =
  DetachTrafficSources'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      trafficSources = Prelude.mempty
    }

-- | The name of the Auto Scaling group.
detachTrafficSources_autoScalingGroupName :: Lens.Lens' DetachTrafficSources Prelude.Text
detachTrafficSources_autoScalingGroupName = Lens.lens (\DetachTrafficSources' {autoScalingGroupName} -> autoScalingGroupName) (\s@DetachTrafficSources' {} a -> s {autoScalingGroupName = a} :: DetachTrafficSources)

-- | The unique identifiers of one or more traffic sources you are detaching.
-- You can specify up to 10 traffic sources.
--
-- Currently, you must specify an Amazon Resource Name (ARN) for an
-- existing VPC Lattice target group. When you detach a target group, it
-- enters the @Removing@ state while deregistering the instances in the
-- group. When all instances are deregistered, then you can no longer
-- describe the target group using the DescribeTrafficSources API call. The
-- instances continue to run.
detachTrafficSources_trafficSources :: Lens.Lens' DetachTrafficSources [TrafficSourceIdentifier]
detachTrafficSources_trafficSources = Lens.lens (\DetachTrafficSources' {trafficSources} -> trafficSources) (\s@DetachTrafficSources' {} a -> s {trafficSources = a} :: DetachTrafficSources) Prelude.. Lens.coerced

instance Core.AWSRequest DetachTrafficSources where
  type
    AWSResponse DetachTrafficSources =
      DetachTrafficSourcesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DetachTrafficSourcesResult"
      ( \s h x ->
          DetachTrafficSourcesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachTrafficSources where
  hashWithSalt _salt DetachTrafficSources' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` trafficSources

instance Prelude.NFData DetachTrafficSources where
  rnf DetachTrafficSources' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf trafficSources

instance Data.ToHeaders DetachTrafficSources where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachTrafficSources where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachTrafficSources where
  toQuery DetachTrafficSources' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DetachTrafficSources" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "TrafficSources"
          Data.=: Data.toQueryList "member" trafficSources
      ]

-- | /See:/ 'newDetachTrafficSourcesResponse' smart constructor.
data DetachTrafficSourcesResponse = DetachTrafficSourcesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachTrafficSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachTrafficSourcesResponse_httpStatus' - The response's http status code.
newDetachTrafficSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachTrafficSourcesResponse
newDetachTrafficSourcesResponse pHttpStatus_ =
  DetachTrafficSourcesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
detachTrafficSourcesResponse_httpStatus :: Lens.Lens' DetachTrafficSourcesResponse Prelude.Int
detachTrafficSourcesResponse_httpStatus = Lens.lens (\DetachTrafficSourcesResponse' {httpStatus} -> httpStatus) (\s@DetachTrafficSourcesResponse' {} a -> s {httpStatus = a} :: DetachTrafficSourcesResponse)

instance Prelude.NFData DetachTrafficSourcesResponse where
  rnf DetachTrafficSourcesResponse' {..} =
    Prelude.rnf httpStatus
