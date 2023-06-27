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
-- Module      : Amazonka.MediaConnect.AddBridgeSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds sources to an existing bridge.
module Amazonka.MediaConnect.AddBridgeSources
  ( -- * Creating a Request
    AddBridgeSources (..),
    newAddBridgeSources,

    -- * Request Lenses
    addBridgeSources_bridgeArn,
    addBridgeSources_sources,

    -- * Destructuring the Response
    AddBridgeSourcesResponse (..),
    newAddBridgeSourcesResponse,

    -- * Response Lenses
    addBridgeSourcesResponse_bridgeArn,
    addBridgeSourcesResponse_sources,
    addBridgeSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to add sources to the specified bridge.
--
-- /See:/ 'newAddBridgeSources' smart constructor.
data AddBridgeSources = AddBridgeSources'
  { -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text,
    -- | The sources that you want to add to this bridge.
    sources :: [AddBridgeSourceRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'addBridgeSources_bridgeArn' - The ARN of the bridge that you want to update.
--
-- 'sources', 'addBridgeSources_sources' - The sources that you want to add to this bridge.
newAddBridgeSources ::
  -- | 'bridgeArn'
  Prelude.Text ->
  AddBridgeSources
newAddBridgeSources pBridgeArn_ =
  AddBridgeSources'
    { bridgeArn = pBridgeArn_,
      sources = Prelude.mempty
    }

-- | The ARN of the bridge that you want to update.
addBridgeSources_bridgeArn :: Lens.Lens' AddBridgeSources Prelude.Text
addBridgeSources_bridgeArn = Lens.lens (\AddBridgeSources' {bridgeArn} -> bridgeArn) (\s@AddBridgeSources' {} a -> s {bridgeArn = a} :: AddBridgeSources)

-- | The sources that you want to add to this bridge.
addBridgeSources_sources :: Lens.Lens' AddBridgeSources [AddBridgeSourceRequest]
addBridgeSources_sources = Lens.lens (\AddBridgeSources' {sources} -> sources) (\s@AddBridgeSources' {} a -> s {sources = a} :: AddBridgeSources) Prelude.. Lens.coerced

instance Core.AWSRequest AddBridgeSources where
  type
    AWSResponse AddBridgeSources =
      AddBridgeSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddBridgeSourcesResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddBridgeSources where
  hashWithSalt _salt AddBridgeSources' {..} =
    _salt
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` sources

instance Prelude.NFData AddBridgeSources where
  rnf AddBridgeSources' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf sources

instance Data.ToHeaders AddBridgeSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddBridgeSources where
  toJSON AddBridgeSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("sources" Data..= sources)]
      )

instance Data.ToPath AddBridgeSources where
  toPath AddBridgeSources' {..} =
    Prelude.mconcat
      ["/v1/bridges/", Data.toBS bridgeArn, "/sources"]

instance Data.ToQuery AddBridgeSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddBridgeSourcesResponse' smart constructor.
data AddBridgeSourcesResponse = AddBridgeSourcesResponse'
  { -- | The Amazon Resource Number (ARN) of the bridge.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    -- | The sources that you added to this bridge.
    sources :: Prelude.Maybe [BridgeSource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'addBridgeSourcesResponse_bridgeArn' - The Amazon Resource Number (ARN) of the bridge.
--
-- 'sources', 'addBridgeSourcesResponse_sources' - The sources that you added to this bridge.
--
-- 'httpStatus', 'addBridgeSourcesResponse_httpStatus' - The response's http status code.
newAddBridgeSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddBridgeSourcesResponse
newAddBridgeSourcesResponse pHttpStatus_ =
  AddBridgeSourcesResponse'
    { bridgeArn =
        Prelude.Nothing,
      sources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the bridge.
addBridgeSourcesResponse_bridgeArn :: Lens.Lens' AddBridgeSourcesResponse (Prelude.Maybe Prelude.Text)
addBridgeSourcesResponse_bridgeArn = Lens.lens (\AddBridgeSourcesResponse' {bridgeArn} -> bridgeArn) (\s@AddBridgeSourcesResponse' {} a -> s {bridgeArn = a} :: AddBridgeSourcesResponse)

-- | The sources that you added to this bridge.
addBridgeSourcesResponse_sources :: Lens.Lens' AddBridgeSourcesResponse (Prelude.Maybe [BridgeSource])
addBridgeSourcesResponse_sources = Lens.lens (\AddBridgeSourcesResponse' {sources} -> sources) (\s@AddBridgeSourcesResponse' {} a -> s {sources = a} :: AddBridgeSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addBridgeSourcesResponse_httpStatus :: Lens.Lens' AddBridgeSourcesResponse Prelude.Int
addBridgeSourcesResponse_httpStatus = Lens.lens (\AddBridgeSourcesResponse' {httpStatus} -> httpStatus) (\s@AddBridgeSourcesResponse' {} a -> s {httpStatus = a} :: AddBridgeSourcesResponse)

instance Prelude.NFData AddBridgeSourcesResponse where
  rnf AddBridgeSourcesResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf httpStatus
