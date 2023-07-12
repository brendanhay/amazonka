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
-- Module      : Amazonka.GuardDuty.CreateDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a single Amazon GuardDuty detector. A detector is a resource
-- that represents the GuardDuty service. To start using GuardDuty, you
-- must create a detector in each Region where you enable the service. You
-- can have only one detector per account per Region. All data sources are
-- enabled in a new detector by default.
module Amazonka.GuardDuty.CreateDetector
  ( -- * Creating a Request
    CreateDetector (..),
    newCreateDetector,

    -- * Request Lenses
    createDetector_clientToken,
    createDetector_dataSources,
    createDetector_findingPublishingFrequency,
    createDetector_tags,
    createDetector_enable,

    -- * Destructuring the Response
    CreateDetectorResponse (..),
    newCreateDetectorResponse,

    -- * Response Lenses
    createDetectorResponse_detectorId,
    createDetectorResponse_unprocessedDataSources,
    createDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDetector' smart constructor.
data CreateDetector = CreateDetector'
  { -- | The idempotency token for the create request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Describes which data sources will be enabled for the detector.
    dataSources :: Prelude.Maybe DataSourceConfigurations,
    -- | A value that specifies how frequently updated findings are exported.
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | The tags to be added to a new detector resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'clientToken', 'createDetector_clientToken' - The idempotency token for the create request.
--
-- 'dataSources', 'createDetector_dataSources' - Describes which data sources will be enabled for the detector.
--
-- 'findingPublishingFrequency', 'createDetector_findingPublishingFrequency' - A value that specifies how frequently updated findings are exported.
--
-- 'tags', 'createDetector_tags' - The tags to be added to a new detector resource.
--
-- 'enable', 'createDetector_enable' - A Boolean value that specifies whether the detector is to be enabled.
newCreateDetector ::
  -- | 'enable'
  Prelude.Bool ->
  CreateDetector
newCreateDetector pEnable_ =
  CreateDetector'
    { clientToken = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      tags = Prelude.Nothing,
      enable = pEnable_
    }

-- | The idempotency token for the create request.
createDetector_clientToken :: Lens.Lens' CreateDetector (Prelude.Maybe Prelude.Text)
createDetector_clientToken = Lens.lens (\CreateDetector' {clientToken} -> clientToken) (\s@CreateDetector' {} a -> s {clientToken = a} :: CreateDetector)

-- | Describes which data sources will be enabled for the detector.
createDetector_dataSources :: Lens.Lens' CreateDetector (Prelude.Maybe DataSourceConfigurations)
createDetector_dataSources = Lens.lens (\CreateDetector' {dataSources} -> dataSources) (\s@CreateDetector' {} a -> s {dataSources = a} :: CreateDetector)

-- | A value that specifies how frequently updated findings are exported.
createDetector_findingPublishingFrequency :: Lens.Lens' CreateDetector (Prelude.Maybe FindingPublishingFrequency)
createDetector_findingPublishingFrequency = Lens.lens (\CreateDetector' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@CreateDetector' {} a -> s {findingPublishingFrequency = a} :: CreateDetector)

-- | The tags to be added to a new detector resource.
createDetector_tags :: Lens.Lens' CreateDetector (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDetector_tags = Lens.lens (\CreateDetector' {tags} -> tags) (\s@CreateDetector' {} a -> s {tags = a} :: CreateDetector) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that specifies whether the detector is to be enabled.
createDetector_enable :: Lens.Lens' CreateDetector Prelude.Bool
createDetector_enable = Lens.lens (\CreateDetector' {enable} -> enable) (\s@CreateDetector' {} a -> s {enable = a} :: CreateDetector)

instance Core.AWSRequest CreateDetector where
  type
    AWSResponse CreateDetector =
      CreateDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDetectorResponse'
            Prelude.<$> (x Data..?> "detectorId")
            Prelude.<*> (x Data..?> "unprocessedDataSources")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDetector where
  hashWithSalt _salt CreateDetector' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` findingPublishingFrequency
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` enable

instance Prelude.NFData CreateDetector where
  rnf CreateDetector' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf findingPublishingFrequency
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf enable

instance Data.ToHeaders CreateDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDetector where
  toJSON CreateDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("dataSources" Data..=) Prelude.<$> dataSources,
            ("findingPublishingFrequency" Data..=)
              Prelude.<$> findingPublishingFrequency,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("enable" Data..= enable)
          ]
      )

instance Data.ToPath CreateDetector where
  toPath = Prelude.const "/detector"

instance Data.ToQuery CreateDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDetectorResponse' smart constructor.
data CreateDetectorResponse = CreateDetectorResponse'
  { -- | The unique ID of the created detector.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the data sources that couldn\'t be enabled when GuardDuty was
    -- enabled for the first time.
    unprocessedDataSources :: Prelude.Maybe UnprocessedDataSourcesResult,
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
-- 'unprocessedDataSources', 'createDetectorResponse_unprocessedDataSources' - Specifies the data sources that couldn\'t be enabled when GuardDuty was
-- enabled for the first time.
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
      unprocessedDataSources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the created detector.
createDetectorResponse_detectorId :: Lens.Lens' CreateDetectorResponse (Prelude.Maybe Prelude.Text)
createDetectorResponse_detectorId = Lens.lens (\CreateDetectorResponse' {detectorId} -> detectorId) (\s@CreateDetectorResponse' {} a -> s {detectorId = a} :: CreateDetectorResponse)

-- | Specifies the data sources that couldn\'t be enabled when GuardDuty was
-- enabled for the first time.
createDetectorResponse_unprocessedDataSources :: Lens.Lens' CreateDetectorResponse (Prelude.Maybe UnprocessedDataSourcesResult)
createDetectorResponse_unprocessedDataSources = Lens.lens (\CreateDetectorResponse' {unprocessedDataSources} -> unprocessedDataSources) (\s@CreateDetectorResponse' {} a -> s {unprocessedDataSources = a} :: CreateDetectorResponse)

-- | The response's http status code.
createDetectorResponse_httpStatus :: Lens.Lens' CreateDetectorResponse Prelude.Int
createDetectorResponse_httpStatus = Lens.lens (\CreateDetectorResponse' {httpStatus} -> httpStatus) (\s@CreateDetectorResponse' {} a -> s {httpStatus = a} :: CreateDetectorResponse)

instance Prelude.NFData CreateDetectorResponse where
  rnf CreateDetectorResponse' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf unprocessedDataSources
      `Prelude.seq` Prelude.rnf httpStatus
