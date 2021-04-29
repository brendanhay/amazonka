{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.UpdateDetector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.UpdateDetector
  ( -- * Creating a Request
    UpdateDetector (..),
    newUpdateDetector,

    -- * Request Lenses
    updateDetector_enable,
    updateDetector_dataSources,
    updateDetector_findingPublishingFrequency,
    updateDetector_detectorId,

    -- * Destructuring the Response
    UpdateDetectorResponse (..),
    newUpdateDetectorResponse,

    -- * Response Lenses
    updateDetectorResponse_httpStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDetector' smart constructor.
data UpdateDetector = UpdateDetector'
  { -- | Specifies whether the detector is enabled or not enabled.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | Describes which data sources will be updated.
    dataSources :: Prelude.Maybe DataSourceConfigurations,
    -- | An enum value that specifies how frequently findings are exported, such
    -- as to CloudWatch Events.
    findingPublishingFrequency :: Prelude.Maybe FindingPublishingFrequency,
    -- | The unique ID of the detector to update.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 'updateDetector_enable' - Specifies whether the detector is enabled or not enabled.
--
-- 'dataSources', 'updateDetector_dataSources' - Describes which data sources will be updated.
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
    { enable = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | Specifies whether the detector is enabled or not enabled.
updateDetector_enable :: Lens.Lens' UpdateDetector (Prelude.Maybe Prelude.Bool)
updateDetector_enable = Lens.lens (\UpdateDetector' {enable} -> enable) (\s@UpdateDetector' {} a -> s {enable = a} :: UpdateDetector)

-- | Describes which data sources will be updated.
updateDetector_dataSources :: Lens.Lens' UpdateDetector (Prelude.Maybe DataSourceConfigurations)
updateDetector_dataSources = Lens.lens (\UpdateDetector' {dataSources} -> dataSources) (\s@UpdateDetector' {} a -> s {dataSources = a} :: UpdateDetector)

-- | An enum value that specifies how frequently findings are exported, such
-- as to CloudWatch Events.
updateDetector_findingPublishingFrequency :: Lens.Lens' UpdateDetector (Prelude.Maybe FindingPublishingFrequency)
updateDetector_findingPublishingFrequency = Lens.lens (\UpdateDetector' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@UpdateDetector' {} a -> s {findingPublishingFrequency = a} :: UpdateDetector)

-- | The unique ID of the detector to update.
updateDetector_detectorId :: Lens.Lens' UpdateDetector Prelude.Text
updateDetector_detectorId = Lens.lens (\UpdateDetector' {detectorId} -> detectorId) (\s@UpdateDetector' {} a -> s {detectorId = a} :: UpdateDetector)

instance Prelude.AWSRequest UpdateDetector where
  type Rs UpdateDetector = UpdateDetectorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDetector

instance Prelude.NFData UpdateDetector

instance Prelude.ToHeaders UpdateDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDetector where
  toJSON UpdateDetector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("enable" Prelude..=) Prelude.<$> enable,
            ("dataSources" Prelude..=) Prelude.<$> dataSources,
            ("findingPublishingFrequency" Prelude..=)
              Prelude.<$> findingPublishingFrequency
          ]
      )

instance Prelude.ToPath UpdateDetector where
  toPath UpdateDetector' {..} =
    Prelude.mconcat
      ["/detector/", Prelude.toBS detectorId]

instance Prelude.ToQuery UpdateDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDetectorResponse' smart constructor.
data UpdateDetectorResponse = UpdateDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateDetectorResponse
