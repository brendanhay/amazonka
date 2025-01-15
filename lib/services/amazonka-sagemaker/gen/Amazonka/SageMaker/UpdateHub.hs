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
-- Module      : Amazonka.SageMaker.UpdateHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a hub.
module Amazonka.SageMaker.UpdateHub
  ( -- * Creating a Request
    UpdateHub (..),
    newUpdateHub,

    -- * Request Lenses
    updateHub_hubDescription,
    updateHub_hubDisplayName,
    updateHub_hubSearchKeywords,
    updateHub_hubName,

    -- * Destructuring the Response
    UpdateHubResponse (..),
    newUpdateHubResponse,

    -- * Response Lenses
    updateHubResponse_httpStatus,
    updateHubResponse_hubArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateHub' smart constructor.
data UpdateHub = UpdateHub'
  { -- | A description of the updated hub.
    hubDescription :: Prelude.Maybe Prelude.Text,
    -- | The display name of the hub.
    hubDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The searchable keywords for the hub.
    hubSearchKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The name of the hub to update.
    hubName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHub' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hubDescription', 'updateHub_hubDescription' - A description of the updated hub.
--
-- 'hubDisplayName', 'updateHub_hubDisplayName' - The display name of the hub.
--
-- 'hubSearchKeywords', 'updateHub_hubSearchKeywords' - The searchable keywords for the hub.
--
-- 'hubName', 'updateHub_hubName' - The name of the hub to update.
newUpdateHub ::
  -- | 'hubName'
  Prelude.Text ->
  UpdateHub
newUpdateHub pHubName_ =
  UpdateHub'
    { hubDescription = Prelude.Nothing,
      hubDisplayName = Prelude.Nothing,
      hubSearchKeywords = Prelude.Nothing,
      hubName = pHubName_
    }

-- | A description of the updated hub.
updateHub_hubDescription :: Lens.Lens' UpdateHub (Prelude.Maybe Prelude.Text)
updateHub_hubDescription = Lens.lens (\UpdateHub' {hubDescription} -> hubDescription) (\s@UpdateHub' {} a -> s {hubDescription = a} :: UpdateHub)

-- | The display name of the hub.
updateHub_hubDisplayName :: Lens.Lens' UpdateHub (Prelude.Maybe Prelude.Text)
updateHub_hubDisplayName = Lens.lens (\UpdateHub' {hubDisplayName} -> hubDisplayName) (\s@UpdateHub' {} a -> s {hubDisplayName = a} :: UpdateHub)

-- | The searchable keywords for the hub.
updateHub_hubSearchKeywords :: Lens.Lens' UpdateHub (Prelude.Maybe [Prelude.Text])
updateHub_hubSearchKeywords = Lens.lens (\UpdateHub' {hubSearchKeywords} -> hubSearchKeywords) (\s@UpdateHub' {} a -> s {hubSearchKeywords = a} :: UpdateHub) Prelude.. Lens.mapping Lens.coerced

-- | The name of the hub to update.
updateHub_hubName :: Lens.Lens' UpdateHub Prelude.Text
updateHub_hubName = Lens.lens (\UpdateHub' {hubName} -> hubName) (\s@UpdateHub' {} a -> s {hubName = a} :: UpdateHub)

instance Core.AWSRequest UpdateHub where
  type AWSResponse UpdateHub = UpdateHubResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateHubResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HubArn")
      )

instance Prelude.Hashable UpdateHub where
  hashWithSalt _salt UpdateHub' {..} =
    _salt
      `Prelude.hashWithSalt` hubDescription
      `Prelude.hashWithSalt` hubDisplayName
      `Prelude.hashWithSalt` hubSearchKeywords
      `Prelude.hashWithSalt` hubName

instance Prelude.NFData UpdateHub where
  rnf UpdateHub' {..} =
    Prelude.rnf hubDescription `Prelude.seq`
      Prelude.rnf hubDisplayName `Prelude.seq`
        Prelude.rnf hubSearchKeywords `Prelude.seq`
          Prelude.rnf hubName

instance Data.ToHeaders UpdateHub where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateHub" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateHub where
  toJSON UpdateHub' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HubDescription" Data..=)
              Prelude.<$> hubDescription,
            ("HubDisplayName" Data..=)
              Prelude.<$> hubDisplayName,
            ("HubSearchKeywords" Data..=)
              Prelude.<$> hubSearchKeywords,
            Prelude.Just ("HubName" Data..= hubName)
          ]
      )

instance Data.ToPath UpdateHub where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateHub where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHubResponse' smart constructor.
data UpdateHubResponse = UpdateHubResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated hub.
    hubArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHubResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHubResponse_httpStatus' - The response's http status code.
--
-- 'hubArn', 'updateHubResponse_hubArn' - The Amazon Resource Name (ARN) of the updated hub.
newUpdateHubResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hubArn'
  Prelude.Text ->
  UpdateHubResponse
newUpdateHubResponse pHttpStatus_ pHubArn_ =
  UpdateHubResponse'
    { httpStatus = pHttpStatus_,
      hubArn = pHubArn_
    }

-- | The response's http status code.
updateHubResponse_httpStatus :: Lens.Lens' UpdateHubResponse Prelude.Int
updateHubResponse_httpStatus = Lens.lens (\UpdateHubResponse' {httpStatus} -> httpStatus) (\s@UpdateHubResponse' {} a -> s {httpStatus = a} :: UpdateHubResponse)

-- | The Amazon Resource Name (ARN) of the updated hub.
updateHubResponse_hubArn :: Lens.Lens' UpdateHubResponse Prelude.Text
updateHubResponse_hubArn = Lens.lens (\UpdateHubResponse' {hubArn} -> hubArn) (\s@UpdateHubResponse' {} a -> s {hubArn = a} :: UpdateHubResponse)

instance Prelude.NFData UpdateHubResponse where
  rnf UpdateHubResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf hubArn
