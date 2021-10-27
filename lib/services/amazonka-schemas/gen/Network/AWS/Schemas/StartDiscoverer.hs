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
-- Module      : Network.AWS.Schemas.StartDiscoverer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the discoverer
module Network.AWS.Schemas.StartDiscoverer
  ( -- * Creating a Request
    StartDiscoverer (..),
    newStartDiscoverer,

    -- * Request Lenses
    startDiscoverer_discovererId,

    -- * Destructuring the Response
    StartDiscovererResponse (..),
    newStartDiscovererResponse,

    -- * Response Lenses
    startDiscovererResponse_state,
    startDiscovererResponse_discovererId,
    startDiscovererResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Schemas.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDiscovererResponse'
            Prelude.<$> (x Core..?> "State")
            Prelude.<*> (x Core..?> "DiscovererId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDiscoverer

instance Prelude.NFData StartDiscoverer

instance Core.ToHeaders StartDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartDiscoverer where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StartDiscoverer where
  toPath StartDiscoverer' {..} =
    Prelude.mconcat
      [ "/v1/discoverers/id/",
        Core.toBS discovererId,
        "/start"
      ]

instance Core.ToQuery StartDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDiscovererResponse' smart constructor.
data StartDiscovererResponse = StartDiscovererResponse'
  { -- | The state of the discoverer.
    state :: Prelude.Maybe DiscovererState,
    -- | The ID of the discoverer.
    discovererId :: Prelude.Maybe Prelude.Text,
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
-- 'state', 'startDiscovererResponse_state' - The state of the discoverer.
--
-- 'discovererId', 'startDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'httpStatus', 'startDiscovererResponse_httpStatus' - The response's http status code.
newStartDiscovererResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDiscovererResponse
newStartDiscovererResponse pHttpStatus_ =
  StartDiscovererResponse'
    { state = Prelude.Nothing,
      discovererId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the discoverer.
startDiscovererResponse_state :: Lens.Lens' StartDiscovererResponse (Prelude.Maybe DiscovererState)
startDiscovererResponse_state = Lens.lens (\StartDiscovererResponse' {state} -> state) (\s@StartDiscovererResponse' {} a -> s {state = a} :: StartDiscovererResponse)

-- | The ID of the discoverer.
startDiscovererResponse_discovererId :: Lens.Lens' StartDiscovererResponse (Prelude.Maybe Prelude.Text)
startDiscovererResponse_discovererId = Lens.lens (\StartDiscovererResponse' {discovererId} -> discovererId) (\s@StartDiscovererResponse' {} a -> s {discovererId = a} :: StartDiscovererResponse)

-- | The response's http status code.
startDiscovererResponse_httpStatus :: Lens.Lens' StartDiscovererResponse Prelude.Int
startDiscovererResponse_httpStatus = Lens.lens (\StartDiscovererResponse' {httpStatus} -> httpStatus) (\s@StartDiscovererResponse' {} a -> s {httpStatus = a} :: StartDiscovererResponse)

instance Prelude.NFData StartDiscovererResponse
