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
-- Module      : Network.AWS.GuardDuty.CreateDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a single Amazon GuardDuty detector. A detector is a resource
-- that represents the GuardDuty service. To start using GuardDuty, you
-- must create a detector in each Region where you enable the service. You
-- can have only one detector per account per Region. All data sources are
-- enabled in a new detector by default.
module Network.AWS.GuardDuty.CreateDetector
  ( -- * Creating a Request
    CreateDetector (..),
    newCreateDetector,

    -- * Request Lenses
    createDetector_dataSources,
    createDetector_findingPublishingFrequency,
    createDetector_tags,
    createDetector_clientToken,
    createDetector_enable,

    -- * Destructuring the Response
    CreateDetectorResponse (..),
    newCreateDetectorResponse,

    -- * Response Lenses
    createDetectorResponse_detectorId,
    createDetectorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDetector' smart constructor.
data CreateDetector = CreateDetector'
  { -- | Describes which data sources will be enabled for the detector.
    dataSources :: Core.Maybe DataSourceConfigurations,
    -- | A value that specifies how frequently updated findings are exported.
    findingPublishingFrequency :: Core.Maybe FindingPublishingFrequency,
    -- | The tags to be added to a new detector resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The idempotency token for the create request.
    clientToken :: Core.Maybe Core.Text,
    -- | A Boolean value that specifies whether the detector is to be enabled.
    enable :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'createDetector_dataSources' - Describes which data sources will be enabled for the detector.
--
-- 'findingPublishingFrequency', 'createDetector_findingPublishingFrequency' - A value that specifies how frequently updated findings are exported.
--
-- 'tags', 'createDetector_tags' - The tags to be added to a new detector resource.
--
-- 'clientToken', 'createDetector_clientToken' - The idempotency token for the create request.
--
-- 'enable', 'createDetector_enable' - A Boolean value that specifies whether the detector is to be enabled.
newCreateDetector ::
  -- | 'enable'
  Core.Bool ->
  CreateDetector
newCreateDetector pEnable_ =
  CreateDetector'
    { dataSources = Core.Nothing,
      findingPublishingFrequency = Core.Nothing,
      tags = Core.Nothing,
      clientToken = Core.Nothing,
      enable = pEnable_
    }

-- | Describes which data sources will be enabled for the detector.
createDetector_dataSources :: Lens.Lens' CreateDetector (Core.Maybe DataSourceConfigurations)
createDetector_dataSources = Lens.lens (\CreateDetector' {dataSources} -> dataSources) (\s@CreateDetector' {} a -> s {dataSources = a} :: CreateDetector)

-- | A value that specifies how frequently updated findings are exported.
createDetector_findingPublishingFrequency :: Lens.Lens' CreateDetector (Core.Maybe FindingPublishingFrequency)
createDetector_findingPublishingFrequency = Lens.lens (\CreateDetector' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@CreateDetector' {} a -> s {findingPublishingFrequency = a} :: CreateDetector)

-- | The tags to be added to a new detector resource.
createDetector_tags :: Lens.Lens' CreateDetector (Core.Maybe (Core.HashMap Core.Text Core.Text))
createDetector_tags = Lens.lens (\CreateDetector' {tags} -> tags) (\s@CreateDetector' {} a -> s {tags = a} :: CreateDetector) Core.. Lens.mapping Lens._Coerce

-- | The idempotency token for the create request.
createDetector_clientToken :: Lens.Lens' CreateDetector (Core.Maybe Core.Text)
createDetector_clientToken = Lens.lens (\CreateDetector' {clientToken} -> clientToken) (\s@CreateDetector' {} a -> s {clientToken = a} :: CreateDetector)

-- | A Boolean value that specifies whether the detector is to be enabled.
createDetector_enable :: Lens.Lens' CreateDetector Core.Bool
createDetector_enable = Lens.lens (\CreateDetector' {enable} -> enable) (\s@CreateDetector' {} a -> s {enable = a} :: CreateDetector)

instance Core.AWSRequest CreateDetector where
  type
    AWSResponse CreateDetector =
      CreateDetectorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDetectorResponse'
            Core.<$> (x Core..?> "detectorId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDetector

instance Core.NFData CreateDetector

instance Core.ToHeaders CreateDetector where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDetector where
  toJSON CreateDetector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("dataSources" Core..=) Core.<$> dataSources,
            ("findingPublishingFrequency" Core..=)
              Core.<$> findingPublishingFrequency,
            ("tags" Core..=) Core.<$> tags,
            ("clientToken" Core..=) Core.<$> clientToken,
            Core.Just ("enable" Core..= enable)
          ]
      )

instance Core.ToPath CreateDetector where
  toPath = Core.const "/detector"

instance Core.ToQuery CreateDetector where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDetectorResponse' smart constructor.
data CreateDetectorResponse = CreateDetectorResponse'
  { -- | The unique ID of the created detector.
    detectorId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'createDetectorResponse_detectorId' - The unique ID of the created detector.
--
-- 'httpStatus', 'createDetectorResponse_httpStatus' - The response's http status code.
newCreateDetectorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDetectorResponse
newCreateDetectorResponse pHttpStatus_ =
  CreateDetectorResponse'
    { detectorId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the created detector.
createDetectorResponse_detectorId :: Lens.Lens' CreateDetectorResponse (Core.Maybe Core.Text)
createDetectorResponse_detectorId = Lens.lens (\CreateDetectorResponse' {detectorId} -> detectorId) (\s@CreateDetectorResponse' {} a -> s {detectorId = a} :: CreateDetectorResponse)

-- | The response's http status code.
createDetectorResponse_httpStatus :: Lens.Lens' CreateDetectorResponse Core.Int
createDetectorResponse_httpStatus = Lens.lens (\CreateDetectorResponse' {httpStatus} -> httpStatus) (\s@CreateDetectorResponse' {} a -> s {httpStatus = a} :: CreateDetectorResponse)

instance Core.NFData CreateDetectorResponse
