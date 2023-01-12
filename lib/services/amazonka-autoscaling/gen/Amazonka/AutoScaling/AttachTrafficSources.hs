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
-- Module      : Amazonka.AutoScaling.AttachTrafficSources
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
-- Attaches one or more traffic sources to the specified Auto Scaling
-- group.
--
-- To describe the traffic sources for an Auto Scaling group, call the
-- DescribeTrafficSources API. To detach a traffic source from the Auto
-- Scaling group, call the DetachTrafficSources API.
--
-- This operation is additive and does not detach existing traffic sources
-- from the Auto Scaling group.
module Amazonka.AutoScaling.AttachTrafficSources
  ( -- * Creating a Request
    AttachTrafficSources (..),
    newAttachTrafficSources,

    -- * Request Lenses
    attachTrafficSources_autoScalingGroupName,
    attachTrafficSources_trafficSources,

    -- * Destructuring the Response
    AttachTrafficSourcesResponse (..),
    newAttachTrafficSourcesResponse,

    -- * Response Lenses
    attachTrafficSourcesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachTrafficSources' smart constructor.
data AttachTrafficSources = AttachTrafficSources'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The unique identifiers of one or more traffic sources. You can specify
    -- up to 10 traffic sources.
    --
    -- Currently, you must specify an Amazon Resource Name (ARN) for an
    -- existing VPC Lattice target group. Amazon EC2 Auto Scaling registers the
    -- running instances with the attached target groups. The target groups
    -- receive incoming traffic and route requests to one or more registered
    -- targets.
    trafficSources :: [TrafficSourceIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachTrafficSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'attachTrafficSources_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'trafficSources', 'attachTrafficSources_trafficSources' - The unique identifiers of one or more traffic sources. You can specify
-- up to 10 traffic sources.
--
-- Currently, you must specify an Amazon Resource Name (ARN) for an
-- existing VPC Lattice target group. Amazon EC2 Auto Scaling registers the
-- running instances with the attached target groups. The target groups
-- receive incoming traffic and route requests to one or more registered
-- targets.
newAttachTrafficSources ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  AttachTrafficSources
newAttachTrafficSources pAutoScalingGroupName_ =
  AttachTrafficSources'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      trafficSources = Prelude.mempty
    }

-- | The name of the Auto Scaling group.
attachTrafficSources_autoScalingGroupName :: Lens.Lens' AttachTrafficSources Prelude.Text
attachTrafficSources_autoScalingGroupName = Lens.lens (\AttachTrafficSources' {autoScalingGroupName} -> autoScalingGroupName) (\s@AttachTrafficSources' {} a -> s {autoScalingGroupName = a} :: AttachTrafficSources)

-- | The unique identifiers of one or more traffic sources. You can specify
-- up to 10 traffic sources.
--
-- Currently, you must specify an Amazon Resource Name (ARN) for an
-- existing VPC Lattice target group. Amazon EC2 Auto Scaling registers the
-- running instances with the attached target groups. The target groups
-- receive incoming traffic and route requests to one or more registered
-- targets.
attachTrafficSources_trafficSources :: Lens.Lens' AttachTrafficSources [TrafficSourceIdentifier]
attachTrafficSources_trafficSources = Lens.lens (\AttachTrafficSources' {trafficSources} -> trafficSources) (\s@AttachTrafficSources' {} a -> s {trafficSources = a} :: AttachTrafficSources) Prelude.. Lens.coerced

instance Core.AWSRequest AttachTrafficSources where
  type
    AWSResponse AttachTrafficSources =
      AttachTrafficSourcesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AttachTrafficSourcesResult"
      ( \s h x ->
          AttachTrafficSourcesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachTrafficSources where
  hashWithSalt _salt AttachTrafficSources' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` trafficSources

instance Prelude.NFData AttachTrafficSources where
  rnf AttachTrafficSources' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf trafficSources

instance Data.ToHeaders AttachTrafficSources where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachTrafficSources where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachTrafficSources where
  toQuery AttachTrafficSources' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AttachTrafficSources" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "TrafficSources"
          Data.=: Data.toQueryList "member" trafficSources
      ]

-- | /See:/ 'newAttachTrafficSourcesResponse' smart constructor.
data AttachTrafficSourcesResponse = AttachTrafficSourcesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachTrafficSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachTrafficSourcesResponse_httpStatus' - The response's http status code.
newAttachTrafficSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachTrafficSourcesResponse
newAttachTrafficSourcesResponse pHttpStatus_ =
  AttachTrafficSourcesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
attachTrafficSourcesResponse_httpStatus :: Lens.Lens' AttachTrafficSourcesResponse Prelude.Int
attachTrafficSourcesResponse_httpStatus = Lens.lens (\AttachTrafficSourcesResponse' {httpStatus} -> httpStatus) (\s@AttachTrafficSourcesResponse' {} a -> s {httpStatus = a} :: AttachTrafficSourcesResponse)

instance Prelude.NFData AttachTrafficSourcesResponse where
  rnf AttachTrafficSourcesResponse' {..} =
    Prelude.rnf httpStatus
