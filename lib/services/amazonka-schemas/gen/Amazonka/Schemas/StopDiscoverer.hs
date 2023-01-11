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
-- Module      : Amazonka.Schemas.StopDiscoverer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the discoverer
module Amazonka.Schemas.StopDiscoverer
  ( -- * Creating a Request
    StopDiscoverer (..),
    newStopDiscoverer,

    -- * Request Lenses
    stopDiscoverer_discovererId,

    -- * Destructuring the Response
    StopDiscovererResponse (..),
    newStopDiscovererResponse,

    -- * Response Lenses
    stopDiscovererResponse_discovererId,
    stopDiscovererResponse_state,
    stopDiscovererResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newStopDiscoverer' smart constructor.
data StopDiscoverer = StopDiscoverer'
  { -- | The ID of the discoverer.
    discovererId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDiscoverer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discovererId', 'stopDiscoverer_discovererId' - The ID of the discoverer.
newStopDiscoverer ::
  -- | 'discovererId'
  Prelude.Text ->
  StopDiscoverer
newStopDiscoverer pDiscovererId_ =
  StopDiscoverer' {discovererId = pDiscovererId_}

-- | The ID of the discoverer.
stopDiscoverer_discovererId :: Lens.Lens' StopDiscoverer Prelude.Text
stopDiscoverer_discovererId = Lens.lens (\StopDiscoverer' {discovererId} -> discovererId) (\s@StopDiscoverer' {} a -> s {discovererId = a} :: StopDiscoverer)

instance Core.AWSRequest StopDiscoverer where
  type
    AWSResponse StopDiscoverer =
      StopDiscovererResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDiscovererResponse'
            Prelude.<$> (x Data..?> "DiscovererId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopDiscoverer where
  hashWithSalt _salt StopDiscoverer' {..} =
    _salt `Prelude.hashWithSalt` discovererId

instance Prelude.NFData StopDiscoverer where
  rnf StopDiscoverer' {..} = Prelude.rnf discovererId

instance Data.ToHeaders StopDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopDiscoverer where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopDiscoverer where
  toPath StopDiscoverer' {..} =
    Prelude.mconcat
      [ "/v1/discoverers/id/",
        Data.toBS discovererId,
        "/stop"
      ]

instance Data.ToQuery StopDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDiscovererResponse' smart constructor.
data StopDiscovererResponse = StopDiscovererResponse'
  { -- | The ID of the discoverer.
    discovererId :: Prelude.Maybe Prelude.Text,
    -- | The state of the discoverer.
    state :: Prelude.Maybe DiscovererState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDiscovererResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discovererId', 'stopDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'state', 'stopDiscovererResponse_state' - The state of the discoverer.
--
-- 'httpStatus', 'stopDiscovererResponse_httpStatus' - The response's http status code.
newStopDiscovererResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDiscovererResponse
newStopDiscovererResponse pHttpStatus_ =
  StopDiscovererResponse'
    { discovererId =
        Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the discoverer.
stopDiscovererResponse_discovererId :: Lens.Lens' StopDiscovererResponse (Prelude.Maybe Prelude.Text)
stopDiscovererResponse_discovererId = Lens.lens (\StopDiscovererResponse' {discovererId} -> discovererId) (\s@StopDiscovererResponse' {} a -> s {discovererId = a} :: StopDiscovererResponse)

-- | The state of the discoverer.
stopDiscovererResponse_state :: Lens.Lens' StopDiscovererResponse (Prelude.Maybe DiscovererState)
stopDiscovererResponse_state = Lens.lens (\StopDiscovererResponse' {state} -> state) (\s@StopDiscovererResponse' {} a -> s {state = a} :: StopDiscovererResponse)

-- | The response's http status code.
stopDiscovererResponse_httpStatus :: Lens.Lens' StopDiscovererResponse Prelude.Int
stopDiscovererResponse_httpStatus = Lens.lens (\StopDiscovererResponse' {httpStatus} -> httpStatus) (\s@StopDiscovererResponse' {} a -> s {httpStatus = a} :: StopDiscovererResponse)

instance Prelude.NFData StopDiscovererResponse where
  rnf StopDiscovererResponse' {..} =
    Prelude.rnf discovererId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
