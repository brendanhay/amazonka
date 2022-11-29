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
-- Module      : Amazonka.Schemas.UpdateDiscoverer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the discoverer
module Amazonka.Schemas.UpdateDiscoverer
  ( -- * Creating a Request
    UpdateDiscoverer (..),
    newUpdateDiscoverer,

    -- * Request Lenses
    updateDiscoverer_description,
    updateDiscoverer_crossAccount,
    updateDiscoverer_discovererId,

    -- * Destructuring the Response
    UpdateDiscovererResponse (..),
    newUpdateDiscovererResponse,

    -- * Response Lenses
    updateDiscovererResponse_tags,
    updateDiscovererResponse_discovererId,
    updateDiscovererResponse_sourceArn,
    updateDiscovererResponse_state,
    updateDiscovererResponse_description,
    updateDiscovererResponse_crossAccount,
    updateDiscovererResponse_discovererArn,
    updateDiscovererResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newUpdateDiscoverer' smart constructor.
data UpdateDiscoverer = UpdateDiscoverer'
  { -- | The description of the discoverer to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | Support discovery of schemas in events sent to the bus from another
    -- account. (default: true)
    crossAccount :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the discoverer.
    discovererId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDiscoverer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDiscoverer_description' - The description of the discoverer to update.
--
-- 'crossAccount', 'updateDiscoverer_crossAccount' - Support discovery of schemas in events sent to the bus from another
-- account. (default: true)
--
-- 'discovererId', 'updateDiscoverer_discovererId' - The ID of the discoverer.
newUpdateDiscoverer ::
  -- | 'discovererId'
  Prelude.Text ->
  UpdateDiscoverer
newUpdateDiscoverer pDiscovererId_ =
  UpdateDiscoverer'
    { description = Prelude.Nothing,
      crossAccount = Prelude.Nothing,
      discovererId = pDiscovererId_
    }

-- | The description of the discoverer to update.
updateDiscoverer_description :: Lens.Lens' UpdateDiscoverer (Prelude.Maybe Prelude.Text)
updateDiscoverer_description = Lens.lens (\UpdateDiscoverer' {description} -> description) (\s@UpdateDiscoverer' {} a -> s {description = a} :: UpdateDiscoverer)

-- | Support discovery of schemas in events sent to the bus from another
-- account. (default: true)
updateDiscoverer_crossAccount :: Lens.Lens' UpdateDiscoverer (Prelude.Maybe Prelude.Bool)
updateDiscoverer_crossAccount = Lens.lens (\UpdateDiscoverer' {crossAccount} -> crossAccount) (\s@UpdateDiscoverer' {} a -> s {crossAccount = a} :: UpdateDiscoverer)

-- | The ID of the discoverer.
updateDiscoverer_discovererId :: Lens.Lens' UpdateDiscoverer Prelude.Text
updateDiscoverer_discovererId = Lens.lens (\UpdateDiscoverer' {discovererId} -> discovererId) (\s@UpdateDiscoverer' {} a -> s {discovererId = a} :: UpdateDiscoverer)

instance Core.AWSRequest UpdateDiscoverer where
  type
    AWSResponse UpdateDiscoverer =
      UpdateDiscovererResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDiscovererResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "DiscovererId")
            Prelude.<*> (x Core..?> "SourceArn")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "CrossAccount")
            Prelude.<*> (x Core..?> "DiscovererArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDiscoverer where
  hashWithSalt _salt UpdateDiscoverer' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` crossAccount
      `Prelude.hashWithSalt` discovererId

instance Prelude.NFData UpdateDiscoverer where
  rnf UpdateDiscoverer' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf crossAccount
      `Prelude.seq` Prelude.rnf discovererId

instance Core.ToHeaders UpdateDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDiscoverer where
  toJSON UpdateDiscoverer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            ("CrossAccount" Core..=) Prelude.<$> crossAccount
          ]
      )

instance Core.ToPath UpdateDiscoverer where
  toPath UpdateDiscoverer' {..} =
    Prelude.mconcat
      ["/v1/discoverers/id/", Core.toBS discovererId]

instance Core.ToQuery UpdateDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDiscovererResponse' smart constructor.
data UpdateDiscovererResponse = UpdateDiscovererResponse'
  { -- | Tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the discoverer.
    discovererId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event bus.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the discoverer.
    state :: Prelude.Maybe DiscovererState,
    -- | The description of the discoverer.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Status if the discoverer will discover schemas from events sent from
    -- another account.
    crossAccount :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the discoverer.
    discovererArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDiscovererResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateDiscovererResponse_tags' - Tags associated with the resource.
--
-- 'discovererId', 'updateDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'sourceArn', 'updateDiscovererResponse_sourceArn' - The ARN of the event bus.
--
-- 'state', 'updateDiscovererResponse_state' - The state of the discoverer.
--
-- 'description', 'updateDiscovererResponse_description' - The description of the discoverer.
--
-- 'crossAccount', 'updateDiscovererResponse_crossAccount' - The Status if the discoverer will discover schemas from events sent from
-- another account.
--
-- 'discovererArn', 'updateDiscovererResponse_discovererArn' - The ARN of the discoverer.
--
-- 'httpStatus', 'updateDiscovererResponse_httpStatus' - The response's http status code.
newUpdateDiscovererResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDiscovererResponse
newUpdateDiscovererResponse pHttpStatus_ =
  UpdateDiscovererResponse'
    { tags = Prelude.Nothing,
      discovererId = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      crossAccount = Prelude.Nothing,
      discovererArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tags associated with the resource.
updateDiscovererResponse_tags :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateDiscovererResponse_tags = Lens.lens (\UpdateDiscovererResponse' {tags} -> tags) (\s@UpdateDiscovererResponse' {} a -> s {tags = a} :: UpdateDiscovererResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the discoverer.
updateDiscovererResponse_discovererId :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe Prelude.Text)
updateDiscovererResponse_discovererId = Lens.lens (\UpdateDiscovererResponse' {discovererId} -> discovererId) (\s@UpdateDiscovererResponse' {} a -> s {discovererId = a} :: UpdateDiscovererResponse)

-- | The ARN of the event bus.
updateDiscovererResponse_sourceArn :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe Prelude.Text)
updateDiscovererResponse_sourceArn = Lens.lens (\UpdateDiscovererResponse' {sourceArn} -> sourceArn) (\s@UpdateDiscovererResponse' {} a -> s {sourceArn = a} :: UpdateDiscovererResponse)

-- | The state of the discoverer.
updateDiscovererResponse_state :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe DiscovererState)
updateDiscovererResponse_state = Lens.lens (\UpdateDiscovererResponse' {state} -> state) (\s@UpdateDiscovererResponse' {} a -> s {state = a} :: UpdateDiscovererResponse)

-- | The description of the discoverer.
updateDiscovererResponse_description :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe Prelude.Text)
updateDiscovererResponse_description = Lens.lens (\UpdateDiscovererResponse' {description} -> description) (\s@UpdateDiscovererResponse' {} a -> s {description = a} :: UpdateDiscovererResponse)

-- | The Status if the discoverer will discover schemas from events sent from
-- another account.
updateDiscovererResponse_crossAccount :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe Prelude.Bool)
updateDiscovererResponse_crossAccount = Lens.lens (\UpdateDiscovererResponse' {crossAccount} -> crossAccount) (\s@UpdateDiscovererResponse' {} a -> s {crossAccount = a} :: UpdateDiscovererResponse)

-- | The ARN of the discoverer.
updateDiscovererResponse_discovererArn :: Lens.Lens' UpdateDiscovererResponse (Prelude.Maybe Prelude.Text)
updateDiscovererResponse_discovererArn = Lens.lens (\UpdateDiscovererResponse' {discovererArn} -> discovererArn) (\s@UpdateDiscovererResponse' {} a -> s {discovererArn = a} :: UpdateDiscovererResponse)

-- | The response's http status code.
updateDiscovererResponse_httpStatus :: Lens.Lens' UpdateDiscovererResponse Prelude.Int
updateDiscovererResponse_httpStatus = Lens.lens (\UpdateDiscovererResponse' {httpStatus} -> httpStatus) (\s@UpdateDiscovererResponse' {} a -> s {httpStatus = a} :: UpdateDiscovererResponse)

instance Prelude.NFData UpdateDiscovererResponse where
  rnf UpdateDiscovererResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf discovererId
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf crossAccount
      `Prelude.seq` Prelude.rnf discovererArn
      `Prelude.seq` Prelude.rnf httpStatus
