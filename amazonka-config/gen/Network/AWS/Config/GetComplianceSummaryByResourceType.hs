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
-- Module      : Network.AWS.Config.GetComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of resources that are compliant and the number that
-- are noncompliant. You can specify one or more resource types to get
-- these numbers for each resource type. The maximum number returned is
-- 100.
module Network.AWS.Config.GetComplianceSummaryByResourceType
  ( -- * Creating a Request
    GetComplianceSummaryByResourceType (..),
    newGetComplianceSummaryByResourceType,

    -- * Request Lenses
    getComplianceSummaryByResourceType_resourceTypes,

    -- * Destructuring the Response
    GetComplianceSummaryByResourceTypeResponse (..),
    newGetComplianceSummaryByResourceTypeResponse,

    -- * Response Lenses
    getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType,
    getComplianceSummaryByResourceTypeResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newGetComplianceSummaryByResourceType' smart constructor.
data GetComplianceSummaryByResourceType = GetComplianceSummaryByResourceType'
  { -- | Specify one or more resource types to get the number of resources that
    -- are compliant and the number that are noncompliant for each resource
    -- type.
    --
    -- For this request, you can specify an AWS resource type such as
    -- @AWS::EC2::Instance@. You can specify that the resource type is an AWS
    -- account by specifying @AWS::::Account@.
    resourceTypes :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceSummaryByResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypes', 'getComplianceSummaryByResourceType_resourceTypes' - Specify one or more resource types to get the number of resources that
-- are compliant and the number that are noncompliant for each resource
-- type.
--
-- For this request, you can specify an AWS resource type such as
-- @AWS::EC2::Instance@. You can specify that the resource type is an AWS
-- account by specifying @AWS::::Account@.
newGetComplianceSummaryByResourceType ::
  GetComplianceSummaryByResourceType
newGetComplianceSummaryByResourceType =
  GetComplianceSummaryByResourceType'
    { resourceTypes =
        Core.Nothing
    }

-- | Specify one or more resource types to get the number of resources that
-- are compliant and the number that are noncompliant for each resource
-- type.
--
-- For this request, you can specify an AWS resource type such as
-- @AWS::EC2::Instance@. You can specify that the resource type is an AWS
-- account by specifying @AWS::::Account@.
getComplianceSummaryByResourceType_resourceTypes :: Lens.Lens' GetComplianceSummaryByResourceType (Core.Maybe [Core.Text])
getComplianceSummaryByResourceType_resourceTypes = Lens.lens (\GetComplianceSummaryByResourceType' {resourceTypes} -> resourceTypes) (\s@GetComplianceSummaryByResourceType' {} a -> s {resourceTypes = a} :: GetComplianceSummaryByResourceType) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSRequest
    GetComplianceSummaryByResourceType
  where
  type
    AWSResponse GetComplianceSummaryByResourceType =
      GetComplianceSummaryByResourceTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceSummaryByResourceTypeResponse'
            Core.<$> ( x Core..?> "ComplianceSummariesByResourceType"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetComplianceSummaryByResourceType

instance
  Core.NFData
    GetComplianceSummaryByResourceType

instance
  Core.ToHeaders
    GetComplianceSummaryByResourceType
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetComplianceSummaryByResourceType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetComplianceSummaryByResourceType
  where
  toJSON GetComplianceSummaryByResourceType' {..} =
    Core.object
      ( Core.catMaybes
          [("ResourceTypes" Core..=) Core.<$> resourceTypes]
      )

instance
  Core.ToPath
    GetComplianceSummaryByResourceType
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetComplianceSummaryByResourceType
  where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newGetComplianceSummaryByResourceTypeResponse' smart constructor.
data GetComplianceSummaryByResourceTypeResponse = GetComplianceSummaryByResourceTypeResponse'
  { -- | The number of resources that are compliant and the number that are
    -- noncompliant. If one or more resource types were provided with the
    -- request, the numbers are returned for each resource type. The maximum
    -- number returned is 100.
    complianceSummariesByResourceType :: Core.Maybe [ComplianceSummaryByResourceType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceSummaryByResourceTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummariesByResourceType', 'getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType' - The number of resources that are compliant and the number that are
-- noncompliant. If one or more resource types were provided with the
-- request, the numbers are returned for each resource type. The maximum
-- number returned is 100.
--
-- 'httpStatus', 'getComplianceSummaryByResourceTypeResponse_httpStatus' - The response's http status code.
newGetComplianceSummaryByResourceTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetComplianceSummaryByResourceTypeResponse
newGetComplianceSummaryByResourceTypeResponse
  pHttpStatus_ =
    GetComplianceSummaryByResourceTypeResponse'
      { complianceSummariesByResourceType =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The number of resources that are compliant and the number that are
-- noncompliant. If one or more resource types were provided with the
-- request, the numbers are returned for each resource type. The maximum
-- number returned is 100.
getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType :: Lens.Lens' GetComplianceSummaryByResourceTypeResponse (Core.Maybe [ComplianceSummaryByResourceType])
getComplianceSummaryByResourceTypeResponse_complianceSummariesByResourceType = Lens.lens (\GetComplianceSummaryByResourceTypeResponse' {complianceSummariesByResourceType} -> complianceSummariesByResourceType) (\s@GetComplianceSummaryByResourceTypeResponse' {} a -> s {complianceSummariesByResourceType = a} :: GetComplianceSummaryByResourceTypeResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getComplianceSummaryByResourceTypeResponse_httpStatus :: Lens.Lens' GetComplianceSummaryByResourceTypeResponse Core.Int
getComplianceSummaryByResourceTypeResponse_httpStatus = Lens.lens (\GetComplianceSummaryByResourceTypeResponse' {httpStatus} -> httpStatus) (\s@GetComplianceSummaryByResourceTypeResponse' {} a -> s {httpStatus = a} :: GetComplianceSummaryByResourceTypeResponse)

instance
  Core.NFData
    GetComplianceSummaryByResourceTypeResponse
