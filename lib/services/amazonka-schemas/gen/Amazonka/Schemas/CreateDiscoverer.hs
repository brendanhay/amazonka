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
-- Module      : Amazonka.Schemas.CreateDiscoverer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a discoverer.
module Amazonka.Schemas.CreateDiscoverer
  ( -- * Creating a Request
    CreateDiscoverer (..),
    newCreateDiscoverer,

    -- * Request Lenses
    createDiscoverer_tags,
    createDiscoverer_description,
    createDiscoverer_crossAccount,
    createDiscoverer_sourceArn,

    -- * Destructuring the Response
    CreateDiscovererResponse (..),
    newCreateDiscovererResponse,

    -- * Response Lenses
    createDiscovererResponse_tags,
    createDiscovererResponse_discovererId,
    createDiscovererResponse_sourceArn,
    createDiscovererResponse_state,
    createDiscovererResponse_description,
    createDiscovererResponse_crossAccount,
    createDiscovererResponse_discovererArn,
    createDiscovererResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- |
--
-- /See:/ 'newCreateDiscoverer' smart constructor.
data CreateDiscoverer = CreateDiscoverer'
  { -- | Tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description for the discoverer.
    description :: Prelude.Maybe Prelude.Text,
    -- | Support discovery of schemas in events sent to the bus from another
    -- account. (default: true).
    crossAccount :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the event bus.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDiscoverer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDiscoverer_tags' - Tags associated with the resource.
--
-- 'description', 'createDiscoverer_description' - A description for the discoverer.
--
-- 'crossAccount', 'createDiscoverer_crossAccount' - Support discovery of schemas in events sent to the bus from another
-- account. (default: true).
--
-- 'sourceArn', 'createDiscoverer_sourceArn' - The ARN of the event bus.
newCreateDiscoverer ::
  -- | 'sourceArn'
  Prelude.Text ->
  CreateDiscoverer
newCreateDiscoverer pSourceArn_ =
  CreateDiscoverer'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      crossAccount = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | Tags associated with the resource.
createDiscoverer_tags :: Lens.Lens' CreateDiscoverer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDiscoverer_tags = Lens.lens (\CreateDiscoverer' {tags} -> tags) (\s@CreateDiscoverer' {} a -> s {tags = a} :: CreateDiscoverer) Prelude.. Lens.mapping Lens.coerced

-- | A description for the discoverer.
createDiscoverer_description :: Lens.Lens' CreateDiscoverer (Prelude.Maybe Prelude.Text)
createDiscoverer_description = Lens.lens (\CreateDiscoverer' {description} -> description) (\s@CreateDiscoverer' {} a -> s {description = a} :: CreateDiscoverer)

-- | Support discovery of schemas in events sent to the bus from another
-- account. (default: true).
createDiscoverer_crossAccount :: Lens.Lens' CreateDiscoverer (Prelude.Maybe Prelude.Bool)
createDiscoverer_crossAccount = Lens.lens (\CreateDiscoverer' {crossAccount} -> crossAccount) (\s@CreateDiscoverer' {} a -> s {crossAccount = a} :: CreateDiscoverer)

-- | The ARN of the event bus.
createDiscoverer_sourceArn :: Lens.Lens' CreateDiscoverer Prelude.Text
createDiscoverer_sourceArn = Lens.lens (\CreateDiscoverer' {sourceArn} -> sourceArn) (\s@CreateDiscoverer' {} a -> s {sourceArn = a} :: CreateDiscoverer)

instance Core.AWSRequest CreateDiscoverer where
  type
    AWSResponse CreateDiscoverer =
      CreateDiscovererResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiscovererResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "DiscovererId")
            Prelude.<*> (x Data..?> "SourceArn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "CrossAccount")
            Prelude.<*> (x Data..?> "DiscovererArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDiscoverer where
  hashWithSalt _salt CreateDiscoverer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` crossAccount
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData CreateDiscoverer where
  rnf CreateDiscoverer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf crossAccount
      `Prelude.seq` Prelude.rnf sourceArn

instance Data.ToHeaders CreateDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDiscoverer where
  toJSON CreateDiscoverer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("Description" Data..=) Prelude.<$> description,
            ("CrossAccount" Data..=) Prelude.<$> crossAccount,
            Prelude.Just ("SourceArn" Data..= sourceArn)
          ]
      )

instance Data.ToPath CreateDiscoverer where
  toPath = Prelude.const "/v1/discoverers"

instance Data.ToQuery CreateDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDiscovererResponse' smart constructor.
data CreateDiscovererResponse = CreateDiscovererResponse'
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
-- Create a value of 'CreateDiscovererResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDiscovererResponse_tags' - Tags associated with the resource.
--
-- 'discovererId', 'createDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'sourceArn', 'createDiscovererResponse_sourceArn' - The ARN of the event bus.
--
-- 'state', 'createDiscovererResponse_state' - The state of the discoverer.
--
-- 'description', 'createDiscovererResponse_description' - The description of the discoverer.
--
-- 'crossAccount', 'createDiscovererResponse_crossAccount' - The Status if the discoverer will discover schemas from events sent from
-- another account.
--
-- 'discovererArn', 'createDiscovererResponse_discovererArn' - The ARN of the discoverer.
--
-- 'httpStatus', 'createDiscovererResponse_httpStatus' - The response's http status code.
newCreateDiscovererResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDiscovererResponse
newCreateDiscovererResponse pHttpStatus_ =
  CreateDiscovererResponse'
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
createDiscovererResponse_tags :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDiscovererResponse_tags = Lens.lens (\CreateDiscovererResponse' {tags} -> tags) (\s@CreateDiscovererResponse' {} a -> s {tags = a} :: CreateDiscovererResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the discoverer.
createDiscovererResponse_discovererId :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_discovererId = Lens.lens (\CreateDiscovererResponse' {discovererId} -> discovererId) (\s@CreateDiscovererResponse' {} a -> s {discovererId = a} :: CreateDiscovererResponse)

-- | The ARN of the event bus.
createDiscovererResponse_sourceArn :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_sourceArn = Lens.lens (\CreateDiscovererResponse' {sourceArn} -> sourceArn) (\s@CreateDiscovererResponse' {} a -> s {sourceArn = a} :: CreateDiscovererResponse)

-- | The state of the discoverer.
createDiscovererResponse_state :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe DiscovererState)
createDiscovererResponse_state = Lens.lens (\CreateDiscovererResponse' {state} -> state) (\s@CreateDiscovererResponse' {} a -> s {state = a} :: CreateDiscovererResponse)

-- | The description of the discoverer.
createDiscovererResponse_description :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_description = Lens.lens (\CreateDiscovererResponse' {description} -> description) (\s@CreateDiscovererResponse' {} a -> s {description = a} :: CreateDiscovererResponse)

-- | The Status if the discoverer will discover schemas from events sent from
-- another account.
createDiscovererResponse_crossAccount :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Bool)
createDiscovererResponse_crossAccount = Lens.lens (\CreateDiscovererResponse' {crossAccount} -> crossAccount) (\s@CreateDiscovererResponse' {} a -> s {crossAccount = a} :: CreateDiscovererResponse)

-- | The ARN of the discoverer.
createDiscovererResponse_discovererArn :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_discovererArn = Lens.lens (\CreateDiscovererResponse' {discovererArn} -> discovererArn) (\s@CreateDiscovererResponse' {} a -> s {discovererArn = a} :: CreateDiscovererResponse)

-- | The response's http status code.
createDiscovererResponse_httpStatus :: Lens.Lens' CreateDiscovererResponse Prelude.Int
createDiscovererResponse_httpStatus = Lens.lens (\CreateDiscovererResponse' {httpStatus} -> httpStatus) (\s@CreateDiscovererResponse' {} a -> s {httpStatus = a} :: CreateDiscovererResponse)

instance Prelude.NFData CreateDiscovererResponse where
  rnf CreateDiscovererResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf discovererId
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf crossAccount
      `Prelude.seq` Prelude.rnf discovererArn
      `Prelude.seq` Prelude.rnf httpStatus
