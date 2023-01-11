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
-- Module      : Amazonka.EC2.CreateTrafficMirrorFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EC2.CreateTrafficMirrorFilter
  ( -- * Creating a Request
    CreateTrafficMirrorFilter (..),
    newCreateTrafficMirrorFilter,

    -- * Request Lenses
    createTrafficMirrorFilter_clientToken,
    createTrafficMirrorFilter_description,
    createTrafficMirrorFilter_dryRun,
    createTrafficMirrorFilter_tagSpecifications,

    -- * Destructuring the Response
    CreateTrafficMirrorFilterResponse (..),
    newCreateTrafficMirrorFilterResponse,

    -- * Response Lenses
    createTrafficMirrorFilterResponse_clientToken,
    createTrafficMirrorFilterResponse_trafficMirrorFilter,
    createTrafficMirrorFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTrafficMirrorFilter' smart constructor.
data CreateTrafficMirrorFilter = CreateTrafficMirrorFilter'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the Traffic Mirror filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to a Traffic Mirror filter.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
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
-- 'clientToken', 'createTrafficMirrorFilter_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'createTrafficMirrorFilter_description' - The description of the Traffic Mirror filter.
--
-- 'dryRun', 'createTrafficMirrorFilter_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createTrafficMirrorFilter_tagSpecifications' - The tags to assign to a Traffic Mirror filter.
newCreateTrafficMirrorFilter ::
  CreateTrafficMirrorFilter
newCreateTrafficMirrorFilter =
  CreateTrafficMirrorFilter'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createTrafficMirrorFilter_clientToken :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe Prelude.Text)
createTrafficMirrorFilter_clientToken = Lens.lens (\CreateTrafficMirrorFilter' {clientToken} -> clientToken) (\s@CreateTrafficMirrorFilter' {} a -> s {clientToken = a} :: CreateTrafficMirrorFilter)

-- | The description of the Traffic Mirror filter.
createTrafficMirrorFilter_description :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe Prelude.Text)
createTrafficMirrorFilter_description = Lens.lens (\CreateTrafficMirrorFilter' {description} -> description) (\s@CreateTrafficMirrorFilter' {} a -> s {description = a} :: CreateTrafficMirrorFilter)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorFilter_dryRun :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe Prelude.Bool)
createTrafficMirrorFilter_dryRun = Lens.lens (\CreateTrafficMirrorFilter' {dryRun} -> dryRun) (\s@CreateTrafficMirrorFilter' {} a -> s {dryRun = a} :: CreateTrafficMirrorFilter)

-- | The tags to assign to a Traffic Mirror filter.
createTrafficMirrorFilter_tagSpecifications :: Lens.Lens' CreateTrafficMirrorFilter (Prelude.Maybe [TagSpecification])
createTrafficMirrorFilter_tagSpecifications = Lens.lens (\CreateTrafficMirrorFilter' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorFilter' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorFilter) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateTrafficMirrorFilter where
  type
    AWSResponse CreateTrafficMirrorFilter =
      CreateTrafficMirrorFilterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorFilterResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> (x Data..@? "trafficMirrorFilter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrafficMirrorFilter where
  hashWithSalt _salt CreateTrafficMirrorFilter' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications

instance Prelude.NFData CreateTrafficMirrorFilter where
  rnf CreateTrafficMirrorFilter' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications

instance Data.ToHeaders CreateTrafficMirrorFilter where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTrafficMirrorFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTrafficMirrorFilter where
  toQuery CreateTrafficMirrorFilter' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateTrafficMirrorFilter" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreateTrafficMirrorFilterResponse' smart constructor.
data CreateTrafficMirrorFilterResponse = CreateTrafficMirrorFilterResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Traffic Mirror filter.
    trafficMirrorFilter :: Prelude.Maybe TrafficMirrorFilter,
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
-- 'clientToken', 'createTrafficMirrorFilterResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'trafficMirrorFilter', 'createTrafficMirrorFilterResponse_trafficMirrorFilter' - Information about the Traffic Mirror filter.
--
-- 'httpStatus', 'createTrafficMirrorFilterResponse_httpStatus' - The response's http status code.
newCreateTrafficMirrorFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrafficMirrorFilterResponse
newCreateTrafficMirrorFilterResponse pHttpStatus_ =
  CreateTrafficMirrorFilterResponse'
    { clientToken =
        Prelude.Nothing,
      trafficMirrorFilter = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createTrafficMirrorFilterResponse_clientToken :: Lens.Lens' CreateTrafficMirrorFilterResponse (Prelude.Maybe Prelude.Text)
createTrafficMirrorFilterResponse_clientToken = Lens.lens (\CreateTrafficMirrorFilterResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorFilterResponse)

-- | Information about the Traffic Mirror filter.
createTrafficMirrorFilterResponse_trafficMirrorFilter :: Lens.Lens' CreateTrafficMirrorFilterResponse (Prelude.Maybe TrafficMirrorFilter)
createTrafficMirrorFilterResponse_trafficMirrorFilter = Lens.lens (\CreateTrafficMirrorFilterResponse' {trafficMirrorFilter} -> trafficMirrorFilter) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {trafficMirrorFilter = a} :: CreateTrafficMirrorFilterResponse)

-- | The response's http status code.
createTrafficMirrorFilterResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorFilterResponse Prelude.Int
createTrafficMirrorFilterResponse_httpStatus = Lens.lens (\CreateTrafficMirrorFilterResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorFilterResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorFilterResponse)

instance
  Prelude.NFData
    CreateTrafficMirrorFilterResponse
  where
  rnf CreateTrafficMirrorFilterResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf trafficMirrorFilter
      `Prelude.seq` Prelude.rnf httpStatus
