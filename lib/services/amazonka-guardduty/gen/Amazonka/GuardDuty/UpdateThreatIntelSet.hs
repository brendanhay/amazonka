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
-- Module      : Amazonka.GuardDuty.UpdateThreatIntelSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the ThreatIntelSet specified by the ThreatIntelSet ID.
module Amazonka.GuardDuty.UpdateThreatIntelSet
  ( -- * Creating a Request
    UpdateThreatIntelSet (..),
    newUpdateThreatIntelSet,

    -- * Request Lenses
    updateThreatIntelSet_name,
    updateThreatIntelSet_location,
    updateThreatIntelSet_activate,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,

    -- * Destructuring the Response
    UpdateThreatIntelSetResponse (..),
    newUpdateThreatIntelSetResponse,

    -- * Response Lenses
    updateThreatIntelSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateThreatIntelSet' smart constructor.
data UpdateThreatIntelSet = UpdateThreatIntelSet'
  { -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated URI of the file that contains the ThreateIntelSet.
    location :: Prelude.Maybe Prelude.Text,
    -- | The updated Boolean value that specifies whether the ThreateIntelSet is
    -- active or not.
    activate :: Prelude.Maybe Prelude.Bool,
    -- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet
    -- you want to update.
    detectorId :: Prelude.Text,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    threatIntelSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThreatIntelSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateThreatIntelSet_name' - The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- 'location', 'updateThreatIntelSet_location' - The updated URI of the file that contains the ThreateIntelSet.
--
-- 'activate', 'updateThreatIntelSet_activate' - The updated Boolean value that specifies whether the ThreateIntelSet is
-- active or not.
--
-- 'detectorId', 'updateThreatIntelSet_detectorId' - The detectorID that specifies the GuardDuty service whose ThreatIntelSet
-- you want to update.
--
-- 'threatIntelSetId', 'updateThreatIntelSet_threatIntelSetId' - The unique ID that specifies the ThreatIntelSet that you want to update.
newUpdateThreatIntelSet ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'threatIntelSetId'
  Prelude.Text ->
  UpdateThreatIntelSet
newUpdateThreatIntelSet
  pDetectorId_
  pThreatIntelSetId_ =
    UpdateThreatIntelSet'
      { name = Prelude.Nothing,
        location = Prelude.Nothing,
        activate = Prelude.Nothing,
        detectorId = pDetectorId_,
        threatIntelSetId = pThreatIntelSetId_
      }

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
updateThreatIntelSet_name :: Lens.Lens' UpdateThreatIntelSet (Prelude.Maybe Prelude.Text)
updateThreatIntelSet_name = Lens.lens (\UpdateThreatIntelSet' {name} -> name) (\s@UpdateThreatIntelSet' {} a -> s {name = a} :: UpdateThreatIntelSet)

-- | The updated URI of the file that contains the ThreateIntelSet.
updateThreatIntelSet_location :: Lens.Lens' UpdateThreatIntelSet (Prelude.Maybe Prelude.Text)
updateThreatIntelSet_location = Lens.lens (\UpdateThreatIntelSet' {location} -> location) (\s@UpdateThreatIntelSet' {} a -> s {location = a} :: UpdateThreatIntelSet)

-- | The updated Boolean value that specifies whether the ThreateIntelSet is
-- active or not.
updateThreatIntelSet_activate :: Lens.Lens' UpdateThreatIntelSet (Prelude.Maybe Prelude.Bool)
updateThreatIntelSet_activate = Lens.lens (\UpdateThreatIntelSet' {activate} -> activate) (\s@UpdateThreatIntelSet' {} a -> s {activate = a} :: UpdateThreatIntelSet)

-- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet
-- you want to update.
updateThreatIntelSet_detectorId :: Lens.Lens' UpdateThreatIntelSet Prelude.Text
updateThreatIntelSet_detectorId = Lens.lens (\UpdateThreatIntelSet' {detectorId} -> detectorId) (\s@UpdateThreatIntelSet' {} a -> s {detectorId = a} :: UpdateThreatIntelSet)

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
updateThreatIntelSet_threatIntelSetId :: Lens.Lens' UpdateThreatIntelSet Prelude.Text
updateThreatIntelSet_threatIntelSetId = Lens.lens (\UpdateThreatIntelSet' {threatIntelSetId} -> threatIntelSetId) (\s@UpdateThreatIntelSet' {} a -> s {threatIntelSetId = a} :: UpdateThreatIntelSet)

instance Core.AWSRequest UpdateThreatIntelSet where
  type
    AWSResponse UpdateThreatIntelSet =
      UpdateThreatIntelSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThreatIntelSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThreatIntelSet where
  hashWithSalt _salt UpdateThreatIntelSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` activate
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` threatIntelSetId

instance Prelude.NFData UpdateThreatIntelSet where
  rnf UpdateThreatIntelSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf activate
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf threatIntelSetId

instance Data.ToHeaders UpdateThreatIntelSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateThreatIntelSet where
  toJSON UpdateThreatIntelSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("location" Data..=) Prelude.<$> location,
            ("activate" Data..=) Prelude.<$> activate
          ]
      )

instance Data.ToPath UpdateThreatIntelSet where
  toPath UpdateThreatIntelSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/threatintelset/",
        Data.toBS threatIntelSetId
      ]

instance Data.ToQuery UpdateThreatIntelSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThreatIntelSetResponse' smart constructor.
data UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThreatIntelSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateThreatIntelSetResponse_httpStatus' - The response's http status code.
newUpdateThreatIntelSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateThreatIntelSetResponse
newUpdateThreatIntelSetResponse pHttpStatus_ =
  UpdateThreatIntelSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateThreatIntelSetResponse_httpStatus :: Lens.Lens' UpdateThreatIntelSetResponse Prelude.Int
updateThreatIntelSetResponse_httpStatus = Lens.lens (\UpdateThreatIntelSetResponse' {httpStatus} -> httpStatus) (\s@UpdateThreatIntelSetResponse' {} a -> s {httpStatus = a} :: UpdateThreatIntelSetResponse)

instance Prelude.NFData UpdateThreatIntelSetResponse where
  rnf UpdateThreatIntelSetResponse' {..} =
    Prelude.rnf httpStatus
