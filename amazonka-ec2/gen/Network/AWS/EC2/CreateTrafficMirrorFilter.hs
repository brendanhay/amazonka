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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTrafficMirrorFilter' smart constructor.
data CreateTrafficMirrorFilter = CreateTrafficMirrorFilter'
  { -- | The tags to assign to a Traffic Mirror filter.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The description of the Traffic Mirror filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      description = Prelude.Nothing,
      clientToken = Prelude.Nothing
    }

-- | The tags to assign to a Traffic Mirror filter.
createTrafficMirrorFilter_tagSpecifications :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe [TagSpecification])
createTrafficMirrorFilter_tagSpecifications = Lens.lens (\CreateTrafficMirrorFilter' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorFilter' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorFilter) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorFilter_dryRun :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe Prelude.Bool)
createTrafficMirrorFilter_dryRun = Lens.lens (\CreateTrafficMirrorFilter' {dryRun} -> dryRun) (\s@CreateTrafficMirrorFilter' {} a -> s {dryRun = a} :: CreateTrafficMirrorFilter)

-- | The description of the Traffic Mirror filter.
createTrafficMirrorFilter_description :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe Prelude.Text)
createTrafficMirrorFilter_description = Lens.lens (\CreateTrafficMirrorFilter' {description} -> description) (\s@CreateTrafficMirrorFilter' {} a -> s {description = a} :: CreateTrafficMirrorFilter)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorFilter_clientToken :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..@? "trafficMirrorFilter")
            Prelude.<*> (x Core..@? "clientToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrafficMirrorFilter

instance Prelude.NFData CreateTrafficMirrorFilter

instance Core.ToHeaders CreateTrafficMirrorFilter where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateTrafficMirrorFilter where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTrafficMirrorFilter where
  toQuery CreateTrafficMirrorFilter' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateTrafficMirrorFilter" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Description" Core.=: description,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newCreateTrafficMirrorFilterResponse' smart constructor.
data CreateTrafficMirrorFilterResponse = CreateTrafficMirrorFilterResponse'
  { -- | Information about the Traffic Mirror filter.
    trafficMirrorFilter :: Prelude.Maybe TrafficMirrorFilter,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateTrafficMirrorFilterResponse
newCreateTrafficMirrorFilterResponse pHttpStatus_ =
  CreateTrafficMirrorFilterResponse'
    { trafficMirrorFilter =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror filter.
createTrafficMirrorFilterResponse_trafficMirrorFilter :: Lens.Lens' CreateTrafficMirrorFilterResponse (Prelude.Maybe TrafficMirrorFilter)
createTrafficMirrorFilterResponse_trafficMirrorFilter = Lens.lens (\CreateTrafficMirrorFilterResponse' {trafficMirrorFilter} -> trafficMirrorFilter) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {trafficMirrorFilter = a} :: CreateTrafficMirrorFilterResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorFilterResponse_clientToken :: Lens.Lens' CreateTrafficMirrorFilterResponse (Prelude.Maybe Prelude.Text)
createTrafficMirrorFilterResponse_clientToken = Lens.lens (\CreateTrafficMirrorFilterResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorFilterResponse)

-- | The response's http status code.
createTrafficMirrorFilterResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorFilterResponse Prelude.Int
createTrafficMirrorFilterResponse_httpStatus = Lens.lens (\CreateTrafficMirrorFilterResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorFilterResponse)

instance
  Prelude.NFData
    CreateTrafficMirrorFilterResponse
