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
-- Module      : Amazonka.MediaConnect.UpdateBridgeSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing bridge source.
module Amazonka.MediaConnect.UpdateBridgeSource
  ( -- * Creating a Request
    UpdateBridgeSource (..),
    newUpdateBridgeSource,

    -- * Request Lenses
    updateBridgeSource_flowSource,
    updateBridgeSource_networkSource,
    updateBridgeSource_bridgeArn,
    updateBridgeSource_sourceName,

    -- * Destructuring the Response
    UpdateBridgeSourceResponse (..),
    newUpdateBridgeSourceResponse,

    -- * Response Lenses
    updateBridgeSourceResponse_bridgeArn,
    updateBridgeSourceResponse_source,
    updateBridgeSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The fields that you want to update in the bridge source.
--
-- /See:/ 'newUpdateBridgeSource' smart constructor.
data UpdateBridgeSource = UpdateBridgeSource'
  { flowSource :: Prelude.Maybe UpdateBridgeFlowSourceRequest,
    networkSource :: Prelude.Maybe UpdateBridgeNetworkSourceRequest,
    -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text,
    -- | The name of the source that you want to update.
    sourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowSource', 'updateBridgeSource_flowSource' - Undocumented member.
--
-- 'networkSource', 'updateBridgeSource_networkSource' - Undocumented member.
--
-- 'bridgeArn', 'updateBridgeSource_bridgeArn' - The ARN of the bridge that you want to update.
--
-- 'sourceName', 'updateBridgeSource_sourceName' - The name of the source that you want to update.
newUpdateBridgeSource ::
  -- | 'bridgeArn'
  Prelude.Text ->
  -- | 'sourceName'
  Prelude.Text ->
  UpdateBridgeSource
newUpdateBridgeSource pBridgeArn_ pSourceName_ =
  UpdateBridgeSource'
    { flowSource = Prelude.Nothing,
      networkSource = Prelude.Nothing,
      bridgeArn = pBridgeArn_,
      sourceName = pSourceName_
    }

-- | Undocumented member.
updateBridgeSource_flowSource :: Lens.Lens' UpdateBridgeSource (Prelude.Maybe UpdateBridgeFlowSourceRequest)
updateBridgeSource_flowSource = Lens.lens (\UpdateBridgeSource' {flowSource} -> flowSource) (\s@UpdateBridgeSource' {} a -> s {flowSource = a} :: UpdateBridgeSource)

-- | Undocumented member.
updateBridgeSource_networkSource :: Lens.Lens' UpdateBridgeSource (Prelude.Maybe UpdateBridgeNetworkSourceRequest)
updateBridgeSource_networkSource = Lens.lens (\UpdateBridgeSource' {networkSource} -> networkSource) (\s@UpdateBridgeSource' {} a -> s {networkSource = a} :: UpdateBridgeSource)

-- | The ARN of the bridge that you want to update.
updateBridgeSource_bridgeArn :: Lens.Lens' UpdateBridgeSource Prelude.Text
updateBridgeSource_bridgeArn = Lens.lens (\UpdateBridgeSource' {bridgeArn} -> bridgeArn) (\s@UpdateBridgeSource' {} a -> s {bridgeArn = a} :: UpdateBridgeSource)

-- | The name of the source that you want to update.
updateBridgeSource_sourceName :: Lens.Lens' UpdateBridgeSource Prelude.Text
updateBridgeSource_sourceName = Lens.lens (\UpdateBridgeSource' {sourceName} -> sourceName) (\s@UpdateBridgeSource' {} a -> s {sourceName = a} :: UpdateBridgeSource)

instance Core.AWSRequest UpdateBridgeSource where
  type
    AWSResponse UpdateBridgeSource =
      UpdateBridgeSourceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBridgeSourceResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "source")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBridgeSource where
  hashWithSalt _salt UpdateBridgeSource' {..} =
    _salt
      `Prelude.hashWithSalt` flowSource
      `Prelude.hashWithSalt` networkSource
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` sourceName

instance Prelude.NFData UpdateBridgeSource where
  rnf UpdateBridgeSource' {..} =
    Prelude.rnf flowSource
      `Prelude.seq` Prelude.rnf networkSource
      `Prelude.seq` Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf sourceName

instance Data.ToHeaders UpdateBridgeSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBridgeSource where
  toJSON UpdateBridgeSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("flowSource" Data..=) Prelude.<$> flowSource,
            ("networkSource" Data..=) Prelude.<$> networkSource
          ]
      )

instance Data.ToPath UpdateBridgeSource where
  toPath UpdateBridgeSource' {..} =
    Prelude.mconcat
      [ "/v1/bridges/",
        Data.toBS bridgeArn,
        "/sources/",
        Data.toBS sourceName
      ]

instance Data.ToQuery UpdateBridgeSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBridgeSourceResponse' smart constructor.
data UpdateBridgeSourceResponse = UpdateBridgeSourceResponse'
  { -- | The Amazon Resource Number (ARN) of the bridge.
    bridgeArn :: Prelude.Maybe Prelude.Text,
    source :: Prelude.Maybe BridgeSource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'updateBridgeSourceResponse_bridgeArn' - The Amazon Resource Number (ARN) of the bridge.
--
-- 'source', 'updateBridgeSourceResponse_source' - Undocumented member.
--
-- 'httpStatus', 'updateBridgeSourceResponse_httpStatus' - The response's http status code.
newUpdateBridgeSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBridgeSourceResponse
newUpdateBridgeSourceResponse pHttpStatus_ =
  UpdateBridgeSourceResponse'
    { bridgeArn =
        Prelude.Nothing,
      source = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the bridge.
updateBridgeSourceResponse_bridgeArn :: Lens.Lens' UpdateBridgeSourceResponse (Prelude.Maybe Prelude.Text)
updateBridgeSourceResponse_bridgeArn = Lens.lens (\UpdateBridgeSourceResponse' {bridgeArn} -> bridgeArn) (\s@UpdateBridgeSourceResponse' {} a -> s {bridgeArn = a} :: UpdateBridgeSourceResponse)

-- | Undocumented member.
updateBridgeSourceResponse_source :: Lens.Lens' UpdateBridgeSourceResponse (Prelude.Maybe BridgeSource)
updateBridgeSourceResponse_source = Lens.lens (\UpdateBridgeSourceResponse' {source} -> source) (\s@UpdateBridgeSourceResponse' {} a -> s {source = a} :: UpdateBridgeSourceResponse)

-- | The response's http status code.
updateBridgeSourceResponse_httpStatus :: Lens.Lens' UpdateBridgeSourceResponse Prelude.Int
updateBridgeSourceResponse_httpStatus = Lens.lens (\UpdateBridgeSourceResponse' {httpStatus} -> httpStatus) (\s@UpdateBridgeSourceResponse' {} a -> s {httpStatus = a} :: UpdateBridgeSourceResponse)

instance Prelude.NFData UpdateBridgeSourceResponse where
  rnf UpdateBridgeSourceResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf httpStatus
