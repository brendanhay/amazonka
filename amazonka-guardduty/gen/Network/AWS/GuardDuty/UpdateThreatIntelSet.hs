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
-- Module      : Network.AWS.GuardDuty.UpdateThreatIntelSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.UpdateThreatIntelSet
  ( -- * Creating a Request
    UpdateThreatIntelSet (..),
    newUpdateThreatIntelSet,

    -- * Request Lenses
    updateThreatIntelSet_activate,
    updateThreatIntelSet_name,
    updateThreatIntelSet_location,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,

    -- * Destructuring the Response
    UpdateThreatIntelSetResponse (..),
    newUpdateThreatIntelSetResponse,

    -- * Response Lenses
    updateThreatIntelSetResponse_httpStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateThreatIntelSet' smart constructor.
data UpdateThreatIntelSet = UpdateThreatIntelSet'
  { -- | The updated Boolean value that specifies whether the ThreateIntelSet is
    -- active or not.
    activate :: Prelude.Maybe Prelude.Bool,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated URI of the file that contains the ThreateIntelSet.
    location :: Prelude.Maybe Prelude.Text,
    -- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet
    -- you want to update.
    detectorId :: Prelude.Text,
    -- | The unique ID that specifies the ThreatIntelSet that you want to update.
    threatIntelSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateThreatIntelSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activate', 'updateThreatIntelSet_activate' - The updated Boolean value that specifies whether the ThreateIntelSet is
-- active or not.
--
-- 'name', 'updateThreatIntelSet_name' - The unique ID that specifies the ThreatIntelSet that you want to update.
--
-- 'location', 'updateThreatIntelSet_location' - The updated URI of the file that contains the ThreateIntelSet.
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
      { activate = Prelude.Nothing,
        name = Prelude.Nothing,
        location = Prelude.Nothing,
        detectorId = pDetectorId_,
        threatIntelSetId = pThreatIntelSetId_
      }

-- | The updated Boolean value that specifies whether the ThreateIntelSet is
-- active or not.
updateThreatIntelSet_activate :: Lens.Lens' UpdateThreatIntelSet (Prelude.Maybe Prelude.Bool)
updateThreatIntelSet_activate = Lens.lens (\UpdateThreatIntelSet' {activate} -> activate) (\s@UpdateThreatIntelSet' {} a -> s {activate = a} :: UpdateThreatIntelSet)

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
updateThreatIntelSet_name :: Lens.Lens' UpdateThreatIntelSet (Prelude.Maybe Prelude.Text)
updateThreatIntelSet_name = Lens.lens (\UpdateThreatIntelSet' {name} -> name) (\s@UpdateThreatIntelSet' {} a -> s {name = a} :: UpdateThreatIntelSet)

-- | The updated URI of the file that contains the ThreateIntelSet.
updateThreatIntelSet_location :: Lens.Lens' UpdateThreatIntelSet (Prelude.Maybe Prelude.Text)
updateThreatIntelSet_location = Lens.lens (\UpdateThreatIntelSet' {location} -> location) (\s@UpdateThreatIntelSet' {} a -> s {location = a} :: UpdateThreatIntelSet)

-- | The detectorID that specifies the GuardDuty service whose ThreatIntelSet
-- you want to update.
updateThreatIntelSet_detectorId :: Lens.Lens' UpdateThreatIntelSet Prelude.Text
updateThreatIntelSet_detectorId = Lens.lens (\UpdateThreatIntelSet' {detectorId} -> detectorId) (\s@UpdateThreatIntelSet' {} a -> s {detectorId = a} :: UpdateThreatIntelSet)

-- | The unique ID that specifies the ThreatIntelSet that you want to update.
updateThreatIntelSet_threatIntelSetId :: Lens.Lens' UpdateThreatIntelSet Prelude.Text
updateThreatIntelSet_threatIntelSetId = Lens.lens (\UpdateThreatIntelSet' {threatIntelSetId} -> threatIntelSetId) (\s@UpdateThreatIntelSet' {} a -> s {threatIntelSetId = a} :: UpdateThreatIntelSet)

instance Prelude.AWSRequest UpdateThreatIntelSet where
  type
    Rs UpdateThreatIntelSet =
      UpdateThreatIntelSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateThreatIntelSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateThreatIntelSet

instance Prelude.NFData UpdateThreatIntelSet

instance Prelude.ToHeaders UpdateThreatIntelSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateThreatIntelSet where
  toJSON UpdateThreatIntelSet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("activate" Prelude..=) Prelude.<$> activate,
            ("name" Prelude..=) Prelude.<$> name,
            ("location" Prelude..=) Prelude.<$> location
          ]
      )

instance Prelude.ToPath UpdateThreatIntelSet where
  toPath UpdateThreatIntelSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/threatintelset/",
        Prelude.toBS threatIntelSetId
      ]

instance Prelude.ToQuery UpdateThreatIntelSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateThreatIntelSetResponse' smart constructor.
data UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateThreatIntelSetResponse
