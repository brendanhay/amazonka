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
-- Module      : Amazonka.MediaConnect.RemoveBridgeSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a source from a bridge.
module Amazonka.MediaConnect.RemoveBridgeSource
  ( -- * Creating a Request
    RemoveBridgeSource (..),
    newRemoveBridgeSource,

    -- * Request Lenses
    removeBridgeSource_bridgeArn,
    removeBridgeSource_sourceName,

    -- * Destructuring the Response
    RemoveBridgeSourceResponse (..),
    newRemoveBridgeSourceResponse,

    -- * Response Lenses
    removeBridgeSourceResponse_bridgeArn,
    removeBridgeSourceResponse_sourceName,
    removeBridgeSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveBridgeSource' smart constructor.
data RemoveBridgeSource = RemoveBridgeSource'
  { -- | The ARN of the bridge that you want to update.
    bridgeArn :: Prelude.Text,
    -- | The name of the bridge source that you want to remove.
    sourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveBridgeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'removeBridgeSource_bridgeArn' - The ARN of the bridge that you want to update.
--
-- 'sourceName', 'removeBridgeSource_sourceName' - The name of the bridge source that you want to remove.
newRemoveBridgeSource ::
  -- | 'bridgeArn'
  Prelude.Text ->
  -- | 'sourceName'
  Prelude.Text ->
  RemoveBridgeSource
newRemoveBridgeSource pBridgeArn_ pSourceName_ =
  RemoveBridgeSource'
    { bridgeArn = pBridgeArn_,
      sourceName = pSourceName_
    }

-- | The ARN of the bridge that you want to update.
removeBridgeSource_bridgeArn :: Lens.Lens' RemoveBridgeSource Prelude.Text
removeBridgeSource_bridgeArn = Lens.lens (\RemoveBridgeSource' {bridgeArn} -> bridgeArn) (\s@RemoveBridgeSource' {} a -> s {bridgeArn = a} :: RemoveBridgeSource)

-- | The name of the bridge source that you want to remove.
removeBridgeSource_sourceName :: Lens.Lens' RemoveBridgeSource Prelude.Text
removeBridgeSource_sourceName = Lens.lens (\RemoveBridgeSource' {sourceName} -> sourceName) (\s@RemoveBridgeSource' {} a -> s {sourceName = a} :: RemoveBridgeSource)

instance Core.AWSRequest RemoveBridgeSource where
  type
    AWSResponse RemoveBridgeSource =
      RemoveBridgeSourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveBridgeSourceResponse'
            Prelude.<$> (x Data..?> "bridgeArn")
            Prelude.<*> (x Data..?> "sourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveBridgeSource where
  hashWithSalt _salt RemoveBridgeSource' {..} =
    _salt
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` sourceName

instance Prelude.NFData RemoveBridgeSource where
  rnf RemoveBridgeSource' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf sourceName

instance Data.ToHeaders RemoveBridgeSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveBridgeSource where
  toPath RemoveBridgeSource' {..} =
    Prelude.mconcat
      [ "/v1/bridges/",
        Data.toBS bridgeArn,
        "/sources/",
        Data.toBS sourceName
      ]

instance Data.ToQuery RemoveBridgeSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveBridgeSourceResponse' smart constructor.
data RemoveBridgeSourceResponse = RemoveBridgeSourceResponse'
  { bridgeArn :: Prelude.Maybe Prelude.Text,
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveBridgeSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'removeBridgeSourceResponse_bridgeArn' - Undocumented member.
--
-- 'sourceName', 'removeBridgeSourceResponse_sourceName' - Undocumented member.
--
-- 'httpStatus', 'removeBridgeSourceResponse_httpStatus' - The response's http status code.
newRemoveBridgeSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveBridgeSourceResponse
newRemoveBridgeSourceResponse pHttpStatus_ =
  RemoveBridgeSourceResponse'
    { bridgeArn =
        Prelude.Nothing,
      sourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
removeBridgeSourceResponse_bridgeArn :: Lens.Lens' RemoveBridgeSourceResponse (Prelude.Maybe Prelude.Text)
removeBridgeSourceResponse_bridgeArn = Lens.lens (\RemoveBridgeSourceResponse' {bridgeArn} -> bridgeArn) (\s@RemoveBridgeSourceResponse' {} a -> s {bridgeArn = a} :: RemoveBridgeSourceResponse)

-- | Undocumented member.
removeBridgeSourceResponse_sourceName :: Lens.Lens' RemoveBridgeSourceResponse (Prelude.Maybe Prelude.Text)
removeBridgeSourceResponse_sourceName = Lens.lens (\RemoveBridgeSourceResponse' {sourceName} -> sourceName) (\s@RemoveBridgeSourceResponse' {} a -> s {sourceName = a} :: RemoveBridgeSourceResponse)

-- | The response's http status code.
removeBridgeSourceResponse_httpStatus :: Lens.Lens' RemoveBridgeSourceResponse Prelude.Int
removeBridgeSourceResponse_httpStatus = Lens.lens (\RemoveBridgeSourceResponse' {httpStatus} -> httpStatus) (\s@RemoveBridgeSourceResponse' {} a -> s {httpStatus = a} :: RemoveBridgeSourceResponse)

instance Prelude.NFData RemoveBridgeSourceResponse where
  rnf RemoveBridgeSourceResponse' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf httpStatus
