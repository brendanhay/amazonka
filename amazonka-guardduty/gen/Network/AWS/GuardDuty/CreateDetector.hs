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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDetector' smart constructor.
data CreateDetector = CreateDetector'
  { -- | Describes which data sources will be enabled for the detector.
    dataSources :: Prelude.Maybe DataSourceConfigurations,
    -- | A value that specifies how frequently updated findings are exported.
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | The tags to be added to a new detector resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The idempotency token for the create request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that specifies whether the detector is to be enabled.
    enable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Bool ->
  CreateDetector
newCreateDetector pEnable_ =
  CreateDetector'
    { dataSources = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      enable = pEnable_
    }

-- | Describes which data sources will be enabled for the detector.
createDetector_dataSources :: Lens.Lens' CreateDetector (Prelude.Maybe DataSourceConfigurations)
createDetector_dataSources = Lens.lens (\CreateDetector' {dataSources} -> dataSources) (\s@CreateDetector' {} a -> s {dataSources = a} :: CreateDetector)

-- | A value that specifies how frequently updated findings are exported.
createDetector_findingPublishingFrequency :: Lens.Lens' CreateDetector (Prelude.Maybe FindingPublishingFrequency)
createDetector_findingPublishingFrequency = Lens.lens (\CreateDetector' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@CreateDetector' {} a -> s {findingPublishingFrequency = a} :: CreateDetector)

-- | The tags to be added to a new detector resource.
createDetector_tags :: Lens.Lens' CreateDetector (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDetector_tags = Lens.lens (\CreateDetector' {tags} -> tags) (\s@CreateDetector' {} a -> s {tags = a} :: CreateDetector) Prelude.. Lens.mapping Lens._Coerce

-- | The idempotency token for the create request.
createDetector_clientToken :: Lens.Lens' CreateDetector (Prelude.Maybe Prelude.Text)
createDetector_clientToken = Lens.lens (\CreateDetector' {clientToken} -> clientToken) (\s@CreateDetector' {} a -> s {clientToken = a} :: CreateDetector)

-- | A Boolean value that specifies whether the detector is to be enabled.
createDetector_enable :: Lens.Lens' CreateDetector Prelude.Bool
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
            Prelude.<$> (x Core..?> "detectorId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDetector

instance Prelude.NFData CreateDetector

instance Core.ToHeaders CreateDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDetector where
  toJSON CreateDetector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dataSources" Core..=) Prelude.<$> dataSources,
            ("findingPublishingFrequency" Core..=)
              Prelude.<$> findingPublishingFrequency,
            ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("enable" Core..= enable)
          ]
      )

instance Core.ToPath CreateDetector where
  toPath = Prelude.const "/detector"

instance Core.ToQuery CreateDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDetectorResponse' smart constructor.
data CreateDetectorResponse = CreateDetectorResponse'
  { -- | The unique ID of the created detector.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateDetectorResponse
newCreateDetectorResponse pHttpStatus_ =
  CreateDetectorResponse'
    { detectorId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the created detector.
createDetectorResponse_detectorId :: Lens.Lens' CreateDetectorResponse (Prelude.Maybe Prelude.Text)
createDetectorResponse_detectorId = Lens.lens (\CreateDetectorResponse' {detectorId} -> detectorId) (\s@CreateDetectorResponse' {} a -> s {detectorId = a} :: CreateDetectorResponse)

-- | The response's http status code.
createDetectorResponse_httpStatus :: Lens.Lens' CreateDetectorResponse Prelude.Int
createDetectorResponse_httpStatus = Lens.lens (\CreateDetectorResponse' {httpStatus} -> httpStatus) (\s@CreateDetectorResponse' {} a -> s {httpStatus = a} :: CreateDetectorResponse)

instance Prelude.NFData CreateDetectorResponse
