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
-- Module      : Amazonka.GuardDuty.UpdateDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon GuardDuty detector specified by the detectorId.
module Amazonka.GuardDuty.UpdateDetector
  ( -- * Creating a Request
    UpdateDetector (..),
    newUpdateDetector,

    -- * Request Lenses
    updateDetector_dataSources,
    updateDetector_enable,
    updateDetector_findingPublishingFrequency,
    updateDetector_detectorId,

    -- * Destructuring the Response
    UpdateDetectorResponse (..),
    newUpdateDetectorResponse,

    -- * Response Lenses
    updateDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDetector' smart constructor.
data UpdateDetector = UpdateDetector'
  { -- | Describes which data sources will be updated.
    dataSources :: Prelude.Maybe DataSourceConfigurations,
    -- | Specifies whether the detector is enabled or not enabled.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | An enum value that specifies how frequently findings are exported, such
    -- as to CloudWatch Events.
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | The unique ID of the detector to update.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'updateDetector_dataSources' - Describes which data sources will be updated.
--
-- 'enable', 'updateDetector_enable' - Specifies whether the detector is enabled or not enabled.
--
-- 'findingPublishingFrequency', 'updateDetector_findingPublishingFrequency' - An enum value that specifies how frequently findings are exported, such
-- as to CloudWatch Events.
--
-- 'detectorId', 'updateDetector_detectorId' - The unique ID of the detector to update.
newUpdateDetector ::
  -- | 'detectorId'
  Prelude.Text ->
  UpdateDetector
newUpdateDetector pDetectorId_ =
  UpdateDetector'
    { dataSources = Prelude.Nothing,
      enable = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | Describes which data sources will be updated.
updateDetector_dataSources :: Lens.Lens' UpdateDetector (Prelude.Maybe DataSourceConfigurations)
updateDetector_dataSources = Lens.lens (\UpdateDetector' {dataSources} -> dataSources) (\s@UpdateDetector' {} a -> s {dataSources = a} :: UpdateDetector)

-- | Specifies whether the detector is enabled or not enabled.
updateDetector_enable :: Lens.Lens' UpdateDetector (Prelude.Maybe Prelude.Bool)
updateDetector_enable = Lens.lens (\UpdateDetector' {enable} -> enable) (\s@UpdateDetector' {} a -> s {enable = a} :: UpdateDetector)

-- | An enum value that specifies how frequently findings are exported, such
-- as to CloudWatch Events.
updateDetector_findingPublishingFrequency :: Lens.Lens' UpdateDetector (Prelude.Maybe FindingPublishingFrequency)
updateDetector_findingPublishingFrequency = Lens.lens (\UpdateDetector' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@UpdateDetector' {} a -> s {findingPublishingFrequency = a} :: UpdateDetector)

-- | The unique ID of the detector to update.
updateDetector_detectorId :: Lens.Lens' UpdateDetector Prelude.Text
updateDetector_detectorId = Lens.lens (\UpdateDetector' {detectorId} -> detectorId) (\s@UpdateDetector' {} a -> s {detectorId = a} :: UpdateDetector)

instance Core.AWSRequest UpdateDetector where
  type
    AWSResponse UpdateDetector =
      UpdateDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDetector where
  hashWithSalt _salt UpdateDetector' {..} =
    _salt `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` enable
      `Prelude.hashWithSalt` findingPublishingFrequency
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData UpdateDetector where
  rnf UpdateDetector' {..} =
    Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf enable
      `Prelude.seq` Prelude.rnf findingPublishingFrequency
      `Prelude.seq` Prelude.rnf detectorId

instance Data.ToHeaders UpdateDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDetector where
  toJSON UpdateDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataSources" Data..=) Prelude.<$> dataSources,
            ("enable" Data..=) Prelude.<$> enable,
            ("findingPublishingFrequency" Data..=)
              Prelude.<$> findingPublishingFrequency
          ]
      )

instance Data.ToPath UpdateDetector where
  toPath UpdateDetector' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId]

instance Data.ToQuery UpdateDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDetectorResponse' smart constructor.
data UpdateDetectorResponse = UpdateDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDetectorResponse_httpStatus' - The response's http status code.
newUpdateDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDetectorResponse
newUpdateDetectorResponse pHttpStatus_ =
  UpdateDetectorResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDetectorResponse_httpStatus :: Lens.Lens' UpdateDetectorResponse Prelude.Int
updateDetectorResponse_httpStatus = Lens.lens (\UpdateDetectorResponse' {httpStatus} -> httpStatus) (\s@UpdateDetectorResponse' {} a -> s {httpStatus = a} :: UpdateDetectorResponse)

instance Prelude.NFData UpdateDetectorResponse where
  rnf UpdateDetectorResponse' {..} =
    Prelude.rnf httpStatus
