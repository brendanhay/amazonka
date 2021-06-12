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
-- Module      : Network.AWS.EC2.CreateTrafficMirrorFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror filter.
--
-- A Traffic Mirror filter is a set of rules that defines the traffic to
-- mirror.
--
-- By default, no traffic is mirrored. To mirror traffic, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorFilterRule.htm CreateTrafficMirrorFilterRule>
-- to add Traffic Mirror rules to the filter. The rules you add define what
-- traffic gets mirrored. You can also use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyTrafficMirrorFilterNetworkServices.html ModifyTrafficMirrorFilterNetworkServices>
-- to mirror supported network services.
module Network.AWS.EC2.CreateTrafficMirrorFilter
  ( -- * Creating a Request
    CreateTrafficMirrorFilter (..),
    newCreateTrafficMirrorFilter,

    -- * Request Lenses
    createTrafficMirrorFilter_tagSpecifications,
    createTrafficMirrorFilter_dryRun,
    createTrafficMirrorFilter_description,
    createTrafficMirrorFilter_clientToken,

    -- * Destructuring the Response
    CreateTrafficMirrorFilterResponse (..),
    newCreateTrafficMirrorFilterResponse,

    -- * Response Lenses
    createTrafficMirrorFilterResponse_trafficMirrorFilter,
    createTrafficMirrorFilterResponse_clientToken,
    createTrafficMirrorFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTrafficMirrorFilter' smart constructor.
data CreateTrafficMirrorFilter = CreateTrafficMirrorFilter'
  { -- | The tags to assign to a Traffic Mirror filter.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The description of the Traffic Mirror filter.
    description :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTrafficMirrorFilter_tagSpecifications' - The tags to assign to a Traffic Mirror filter.
--
-- 'dryRun', 'createTrafficMirrorFilter_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'description', 'createTrafficMirrorFilter_description' - The description of the Traffic Mirror filter.
--
-- 'clientToken', 'createTrafficMirrorFilter_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
newCreateTrafficMirrorFilter ::
  CreateTrafficMirrorFilter
newCreateTrafficMirrorFilter =
  CreateTrafficMirrorFilter'
    { tagSpecifications =
        Core.Nothing,
      dryRun = Core.Nothing,
      description = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The tags to assign to a Traffic Mirror filter.
createTrafficMirrorFilter_tagSpecifications :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe [TagSpecification])
createTrafficMirrorFilter_tagSpecifications = Lens.lens (\CreateTrafficMirrorFilter' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorFilter' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorFilter) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorFilter_dryRun :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe Core.Bool)
createTrafficMirrorFilter_dryRun = Lens.lens (\CreateTrafficMirrorFilter' {dryRun} -> dryRun) (\s@CreateTrafficMirrorFilter' {} a -> s {dryRun = a} :: CreateTrafficMirrorFilter)

-- | The description of the Traffic Mirror filter.
createTrafficMirrorFilter_description :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe Core.Text)
createTrafficMirrorFilter_description = Lens.lens (\CreateTrafficMirrorFilter' {description} -> description) (\s@CreateTrafficMirrorFilter' {} a -> s {description = a} :: CreateTrafficMirrorFilter)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorFilter_clientToken :: Lens.Lens' CreateTrafficMirrorFilter (Core.Maybe Core.Text)
createTrafficMirrorFilter_clientToken = Lens.lens (\CreateTrafficMirrorFilter' {clientToken} -> clientToken) (\s@CreateTrafficMirrorFilter' {} a -> s {clientToken = a} :: CreateTrafficMirrorFilter)

instance Core.AWSRequest CreateTrafficMirrorFilter where
  type
    AWSResponse CreateTrafficMirrorFilter =
      CreateTrafficMirrorFilterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorFilterResponse'
            Core.<$> (x Core..@? "trafficMirrorFilter")
            Core.<*> (x Core..@? "clientToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTrafficMirrorFilter

instance Core.NFData CreateTrafficMirrorFilter

instance Core.ToHeaders CreateTrafficMirrorFilter where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTrafficMirrorFilter where
  toPath = Core.const "/"

instance Core.ToQuery CreateTrafficMirrorFilter where
  toQuery CreateTrafficMirrorFilter' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateTrafficMirrorFilter" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Description" Core.=: description,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newCreateTrafficMirrorFilterResponse' smart constructor.
data CreateTrafficMirrorFilterResponse = CreateTrafficMirrorFilterResponse'
  { -- | Information about the Traffic Mirror filter.
    trafficMirrorFilter :: Core.Maybe TrafficMirrorFilter,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilter', 'createTrafficMirrorFilterResponse_trafficMirrorFilter' - Information about the Traffic Mirror filter.
--
-- 'clientToken', 'createTrafficMirrorFilterResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'httpStatus', 'createTrafficMirrorFilterResponse_httpStatus' - The response's http status code.
newCreateTrafficMirrorFilterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTrafficMirrorFilterResponse
newCreateTrafficMirrorFilterResponse pHttpStatus_ =
  CreateTrafficMirrorFilterResponse'
    { trafficMirrorFilter =
        Core.Nothing,
      clientToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror filter.
createTrafficMirrorFilterResponse_trafficMirrorFilter :: Lens.Lens' CreateTrafficMirrorFilterResponse (Core.Maybe TrafficMirrorFilter)
createTrafficMirrorFilterResponse_trafficMirrorFilter = Lens.lens (\CreateTrafficMirrorFilterResponse' {trafficMirrorFilter} -> trafficMirrorFilter) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {trafficMirrorFilter = a} :: CreateTrafficMirrorFilterResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorFilterResponse_clientToken :: Lens.Lens' CreateTrafficMirrorFilterResponse (Core.Maybe Core.Text)
createTrafficMirrorFilterResponse_clientToken = Lens.lens (\CreateTrafficMirrorFilterResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorFilterResponse)

-- | The response's http status code.
createTrafficMirrorFilterResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorFilterResponse Core.Int
createTrafficMirrorFilterResponse_httpStatus = Lens.lens (\CreateTrafficMirrorFilterResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorFilterResponse)

instance
  Core.NFData
    CreateTrafficMirrorFilterResponse
