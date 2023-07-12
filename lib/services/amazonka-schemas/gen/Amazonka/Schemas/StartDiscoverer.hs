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
-- Module      : Amazonka.Schemas.StartDiscoverer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the discoverer
module Amazonka.Schemas.StartDiscoverer
  ( -- * Creating a Request
    StartDiscoverer (..),
    newStartDiscoverer,

    -- * Request Lenses
    startDiscoverer_discovererId,

    -- * Destructuring the Response
    StartDiscovererResponse (..),
    newStartDiscovererResponse,

    -- * Response Lenses
    startDiscovererResponse_discovererId,
    startDiscovererResponse_state,
    startDiscovererResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newStartDiscoverer' smart constructor.
data StartDiscoverer = StartDiscoverer'
  { -- | The ID of the discoverer.
    discovererId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDiscoverer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discovererId', 'startDiscoverer_discovererId' - The ID of the discoverer.
newStartDiscoverer ::
  -- | 'discovererId'
  Prelude.Text ->
  StartDiscoverer
newStartDiscoverer pDiscovererId_ =
  StartDiscoverer' {discovererId = pDiscovererId_}

-- | The ID of the discoverer.
startDiscoverer_discovererId :: Lens.Lens' StartDiscoverer Prelude.Text
startDiscoverer_discovererId = Lens.lens (\StartDiscoverer' {discovererId} -> discovererId) (\s@StartDiscoverer' {} a -> s {discovererId = a} :: StartDiscoverer)

instance Core.AWSRequest StartDiscoverer where
  type
    AWSResponse StartDiscoverer =
      StartDiscovererResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDiscovererResponse'
            Prelude.<$> (x Data..?> "DiscovererId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDiscoverer where
  hashWithSalt _salt StartDiscoverer' {..} =
    _salt `Prelude.hashWithSalt` discovererId

instance Prelude.NFData StartDiscoverer where
  rnf StartDiscoverer' {..} = Prelude.rnf discovererId

instance Data.ToHeaders StartDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDiscoverer where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartDiscoverer where
  toPath StartDiscoverer' {..} =
    Prelude.mconcat
      [ "/v1/discoverers/id/",
        Data.toBS discovererId,
        "/start"
      ]

instance Data.ToQuery StartDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDiscovererResponse' smart constructor.
data StartDiscovererResponse = StartDiscovererResponse'
  { -- | The ID of the discoverer.
    discovererId :: Prelude.Maybe Prelude.Text,
    -- | The state of the discoverer.
    state :: Prelude.Maybe DiscovererState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDiscovererResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discovererId', 'startDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'state', 'startDiscovererResponse_state' - The state of the discoverer.
--
-- 'httpStatus', 'startDiscovererResponse_httpStatus' - The response's http status code.
newStartDiscovererResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDiscovererResponse
newStartDiscovererResponse pHttpStatus_ =
  StartDiscovererResponse'
    { discovererId =
        Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the discoverer.
startDiscovererResponse_discovererId :: Lens.Lens' StartDiscovererResponse (Prelude.Maybe Prelude.Text)
startDiscovererResponse_discovererId = Lens.lens (\StartDiscovererResponse' {discovererId} -> discovererId) (\s@StartDiscovererResponse' {} a -> s {discovererId = a} :: StartDiscovererResponse)

-- | The state of the discoverer.
startDiscovererResponse_state :: Lens.Lens' StartDiscovererResponse (Prelude.Maybe DiscovererState)
startDiscovererResponse_state = Lens.lens (\StartDiscovererResponse' {state} -> state) (\s@StartDiscovererResponse' {} a -> s {state = a} :: StartDiscovererResponse)

-- | The response's http status code.
startDiscovererResponse_httpStatus :: Lens.Lens' StartDiscovererResponse Prelude.Int
startDiscovererResponse_httpStatus = Lens.lens (\StartDiscovererResponse' {httpStatus} -> httpStatus) (\s@StartDiscovererResponse' {} a -> s {httpStatus = a} :: StartDiscovererResponse)

instance Prelude.NFData StartDiscovererResponse where
  rnf StartDiscovererResponse' {..} =
    Prelude.rnf discovererId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
