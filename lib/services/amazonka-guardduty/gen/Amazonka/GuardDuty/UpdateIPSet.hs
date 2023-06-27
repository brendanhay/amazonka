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
-- Module      : Amazonka.GuardDuty.UpdateIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the IPSet specified by the IPSet ID.
module Amazonka.GuardDuty.UpdateIPSet
  ( -- * Creating a Request
    UpdateIPSet (..),
    newUpdateIPSet,

    -- * Request Lenses
    updateIPSet_activate,
    updateIPSet_location,
    updateIPSet_name,
    updateIPSet_detectorId,
    updateIPSet_ipSetId,

    -- * Destructuring the Response
    UpdateIPSetResponse (..),
    newUpdateIPSetResponse,

    -- * Response Lenses
    updateIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { -- | The updated Boolean value that specifies whether the IPSet is active or
    -- not.
    activate :: Prelude.Maybe Prelude.Bool,
    -- | The updated URI of the file that contains the IPSet.
    location :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that specifies the IPSet that you want to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | The detectorID that specifies the GuardDuty service whose IPSet you want
    -- to update.
    detectorId :: Prelude.Text,
    -- | The unique ID that specifies the IPSet that you want to update.
    ipSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activate', 'updateIPSet_activate' - The updated Boolean value that specifies whether the IPSet is active or
-- not.
--
-- 'location', 'updateIPSet_location' - The updated URI of the file that contains the IPSet.
--
-- 'name', 'updateIPSet_name' - The unique ID that specifies the IPSet that you want to update.
--
-- 'detectorId', 'updateIPSet_detectorId' - The detectorID that specifies the GuardDuty service whose IPSet you want
-- to update.
--
-- 'ipSetId', 'updateIPSet_ipSetId' - The unique ID that specifies the IPSet that you want to update.
newUpdateIPSet ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'ipSetId'
  Prelude.Text ->
  UpdateIPSet
newUpdateIPSet pDetectorId_ pIpSetId_ =
  UpdateIPSet'
    { activate = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      detectorId = pDetectorId_,
      ipSetId = pIpSetId_
    }

-- | The updated Boolean value that specifies whether the IPSet is active or
-- not.
updateIPSet_activate :: Lens.Lens' UpdateIPSet (Prelude.Maybe Prelude.Bool)
updateIPSet_activate = Lens.lens (\UpdateIPSet' {activate} -> activate) (\s@UpdateIPSet' {} a -> s {activate = a} :: UpdateIPSet)

-- | The updated URI of the file that contains the IPSet.
updateIPSet_location :: Lens.Lens' UpdateIPSet (Prelude.Maybe Prelude.Text)
updateIPSet_location = Lens.lens (\UpdateIPSet' {location} -> location) (\s@UpdateIPSet' {} a -> s {location = a} :: UpdateIPSet)

-- | The unique ID that specifies the IPSet that you want to update.
updateIPSet_name :: Lens.Lens' UpdateIPSet (Prelude.Maybe Prelude.Text)
updateIPSet_name = Lens.lens (\UpdateIPSet' {name} -> name) (\s@UpdateIPSet' {} a -> s {name = a} :: UpdateIPSet)

-- | The detectorID that specifies the GuardDuty service whose IPSet you want
-- to update.
updateIPSet_detectorId :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_detectorId = Lens.lens (\UpdateIPSet' {detectorId} -> detectorId) (\s@UpdateIPSet' {} a -> s {detectorId = a} :: UpdateIPSet)

-- | The unique ID that specifies the IPSet that you want to update.
updateIPSet_ipSetId :: Lens.Lens' UpdateIPSet Prelude.Text
updateIPSet_ipSetId = Lens.lens (\UpdateIPSet' {ipSetId} -> ipSetId) (\s@UpdateIPSet' {} a -> s {ipSetId = a} :: UpdateIPSet)

instance Core.AWSRequest UpdateIPSet where
  type AWSResponse UpdateIPSet = UpdateIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIPSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIPSet where
  hashWithSalt _salt UpdateIPSet' {..} =
    _salt
      `Prelude.hashWithSalt` activate
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` ipSetId

instance Prelude.NFData UpdateIPSet where
  rnf UpdateIPSet' {..} =
    Prelude.rnf activate
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf ipSetId

instance Data.ToHeaders UpdateIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIPSet where
  toJSON UpdateIPSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("activate" Data..=) Prelude.<$> activate,
            ("location" Data..=) Prelude.<$> location,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateIPSet where
  toPath UpdateIPSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/ipset/",
        Data.toBS ipSetId
      ]

instance Data.ToQuery UpdateIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIPSetResponse_httpStatus' - The response's http status code.
newUpdateIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIPSetResponse
newUpdateIPSetResponse pHttpStatus_ =
  UpdateIPSetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateIPSetResponse_httpStatus :: Lens.Lens' UpdateIPSetResponse Prelude.Int
updateIPSetResponse_httpStatus = Lens.lens (\UpdateIPSetResponse' {httpStatus} -> httpStatus) (\s@UpdateIPSetResponse' {} a -> s {httpStatus = a} :: UpdateIPSetResponse)

instance Prelude.NFData UpdateIPSetResponse where
  rnf UpdateIPSetResponse' {..} = Prelude.rnf httpStatus
