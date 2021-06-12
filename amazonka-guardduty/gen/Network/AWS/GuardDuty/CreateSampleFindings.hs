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
-- Module      : Network.AWS.GuardDuty.CreateSampleFindings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates example findings of types specified by the list of finding
-- types. If \'NULL\' is specified for @findingTypes@, the API generates
-- example findings of all supported finding types.
module Network.AWS.GuardDuty.CreateSampleFindings
  ( -- * Creating a Request
    CreateSampleFindings (..),
    newCreateSampleFindings,

    -- * Request Lenses
    createSampleFindings_findingTypes,
    createSampleFindings_detectorId,

    -- * Destructuring the Response
    CreateSampleFindingsResponse (..),
    newCreateSampleFindingsResponse,

    -- * Response Lenses
    createSampleFindingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSampleFindings' smart constructor.
data CreateSampleFindings = CreateSampleFindings'
  { -- | The types of sample findings to generate.
    findingTypes :: Core.Maybe [Core.Text],
    -- | The ID of the detector to create sample findings for.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSampleFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingTypes', 'createSampleFindings_findingTypes' - The types of sample findings to generate.
--
-- 'detectorId', 'createSampleFindings_detectorId' - The ID of the detector to create sample findings for.
newCreateSampleFindings ::
  -- | 'detectorId'
  Core.Text ->
  CreateSampleFindings
newCreateSampleFindings pDetectorId_ =
  CreateSampleFindings'
    { findingTypes = Core.Nothing,
      detectorId = pDetectorId_
    }

-- | The types of sample findings to generate.
createSampleFindings_findingTypes :: Lens.Lens' CreateSampleFindings (Core.Maybe [Core.Text])
createSampleFindings_findingTypes = Lens.lens (\CreateSampleFindings' {findingTypes} -> findingTypes) (\s@CreateSampleFindings' {} a -> s {findingTypes = a} :: CreateSampleFindings) Core.. Lens.mapping Lens._Coerce

-- | The ID of the detector to create sample findings for.
createSampleFindings_detectorId :: Lens.Lens' CreateSampleFindings Core.Text
createSampleFindings_detectorId = Lens.lens (\CreateSampleFindings' {detectorId} -> detectorId) (\s@CreateSampleFindings' {} a -> s {detectorId = a} :: CreateSampleFindings)

instance Core.AWSRequest CreateSampleFindings where
  type
    AWSResponse CreateSampleFindings =
      CreateSampleFindingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSampleFindingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSampleFindings

instance Core.NFData CreateSampleFindings

instance Core.ToHeaders CreateSampleFindings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSampleFindings where
  toJSON CreateSampleFindings' {..} =
    Core.object
      ( Core.catMaybes
          [("findingTypes" Core..=) Core.<$> findingTypes]
      )

instance Core.ToPath CreateSampleFindings where
  toPath CreateSampleFindings' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/findings/create"
      ]

instance Core.ToQuery CreateSampleFindings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSampleFindingsResponse' smart constructor.
data CreateSampleFindingsResponse = CreateSampleFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSampleFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSampleFindingsResponse_httpStatus' - The response's http status code.
newCreateSampleFindingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSampleFindingsResponse
newCreateSampleFindingsResponse pHttpStatus_ =
  CreateSampleFindingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createSampleFindingsResponse_httpStatus :: Lens.Lens' CreateSampleFindingsResponse Core.Int
createSampleFindingsResponse_httpStatus = Lens.lens (\CreateSampleFindingsResponse' {httpStatus} -> httpStatus) (\s@CreateSampleFindingsResponse' {} a -> s {httpStatus = a} :: CreateSampleFindingsResponse)

instance Core.NFData CreateSampleFindingsResponse
