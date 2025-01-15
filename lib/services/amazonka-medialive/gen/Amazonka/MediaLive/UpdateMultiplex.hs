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
-- Module      : Amazonka.MediaLive.UpdateMultiplex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a multiplex.
module Amazonka.MediaLive.UpdateMultiplex
  ( -- * Creating a Request
    UpdateMultiplex' (..),
    newUpdateMultiplex',

    -- * Request Lenses
    updateMultiplex'_multiplexSettings,
    updateMultiplex'_name,
    updateMultiplex'_multiplexId,

    -- * Destructuring the Response
    UpdateMultiplexResponse (..),
    newUpdateMultiplexResponse,

    -- * Response Lenses
    updateMultiplexResponse_multiplex,
    updateMultiplexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update a multiplex.
--
-- /See:/ 'newUpdateMultiplex'' smart constructor.
data UpdateMultiplex' = UpdateMultiplex''
  { -- | The new settings for a multiplex.
    multiplexSettings :: Prelude.Maybe MultiplexSettings,
    -- | Name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | ID of the multiplex to update.
    multiplexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMultiplex'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexSettings', 'updateMultiplex'_multiplexSettings' - The new settings for a multiplex.
--
-- 'name', 'updateMultiplex'_name' - Name of the multiplex.
--
-- 'multiplexId', 'updateMultiplex'_multiplexId' - ID of the multiplex to update.
newUpdateMultiplex' ::
  -- | 'multiplexId'
  Prelude.Text ->
  UpdateMultiplex'
newUpdateMultiplex' pMultiplexId_ =
  UpdateMultiplex''
    { multiplexSettings =
        Prelude.Nothing,
      name = Prelude.Nothing,
      multiplexId = pMultiplexId_
    }

-- | The new settings for a multiplex.
updateMultiplex'_multiplexSettings :: Lens.Lens' UpdateMultiplex' (Prelude.Maybe MultiplexSettings)
updateMultiplex'_multiplexSettings = Lens.lens (\UpdateMultiplex'' {multiplexSettings} -> multiplexSettings) (\s@UpdateMultiplex'' {} a -> s {multiplexSettings = a} :: UpdateMultiplex')

-- | Name of the multiplex.
updateMultiplex'_name :: Lens.Lens' UpdateMultiplex' (Prelude.Maybe Prelude.Text)
updateMultiplex'_name = Lens.lens (\UpdateMultiplex'' {name} -> name) (\s@UpdateMultiplex'' {} a -> s {name = a} :: UpdateMultiplex')

-- | ID of the multiplex to update.
updateMultiplex'_multiplexId :: Lens.Lens' UpdateMultiplex' Prelude.Text
updateMultiplex'_multiplexId = Lens.lens (\UpdateMultiplex'' {multiplexId} -> multiplexId) (\s@UpdateMultiplex'' {} a -> s {multiplexId = a} :: UpdateMultiplex')

instance Core.AWSRequest UpdateMultiplex' where
  type
    AWSResponse UpdateMultiplex' =
      UpdateMultiplexResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMultiplexResponse'
            Prelude.<$> (x Data..?> "multiplex")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMultiplex' where
  hashWithSalt _salt UpdateMultiplex'' {..} =
    _salt
      `Prelude.hashWithSalt` multiplexSettings
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` multiplexId

instance Prelude.NFData UpdateMultiplex' where
  rnf UpdateMultiplex'' {..} =
    Prelude.rnf multiplexSettings `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf multiplexId

instance Data.ToHeaders UpdateMultiplex' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMultiplex' where
  toJSON UpdateMultiplex'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("multiplexSettings" Data..=)
              Prelude.<$> multiplexSettings,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateMultiplex' where
  toPath UpdateMultiplex'' {..} =
    Prelude.mconcat
      ["/prod/multiplexes/", Data.toBS multiplexId]

instance Data.ToQuery UpdateMultiplex' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateMultiplexResponse
--
-- /See:/ 'newUpdateMultiplexResponse' smart constructor.
data UpdateMultiplexResponse = UpdateMultiplexResponse'
  { -- | The updated multiplex.
    multiplex :: Prelude.Maybe Multiplex,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplex', 'updateMultiplexResponse_multiplex' - The updated multiplex.
--
-- 'httpStatus', 'updateMultiplexResponse_httpStatus' - The response's http status code.
newUpdateMultiplexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMultiplexResponse
newUpdateMultiplexResponse pHttpStatus_ =
  UpdateMultiplexResponse'
    { multiplex =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated multiplex.
updateMultiplexResponse_multiplex :: Lens.Lens' UpdateMultiplexResponse (Prelude.Maybe Multiplex)
updateMultiplexResponse_multiplex = Lens.lens (\UpdateMultiplexResponse' {multiplex} -> multiplex) (\s@UpdateMultiplexResponse' {} a -> s {multiplex = a} :: UpdateMultiplexResponse)

-- | The response's http status code.
updateMultiplexResponse_httpStatus :: Lens.Lens' UpdateMultiplexResponse Prelude.Int
updateMultiplexResponse_httpStatus = Lens.lens (\UpdateMultiplexResponse' {httpStatus} -> httpStatus) (\s@UpdateMultiplexResponse' {} a -> s {httpStatus = a} :: UpdateMultiplexResponse)

instance Prelude.NFData UpdateMultiplexResponse where
  rnf UpdateMultiplexResponse' {..} =
    Prelude.rnf multiplex `Prelude.seq`
      Prelude.rnf httpStatus
