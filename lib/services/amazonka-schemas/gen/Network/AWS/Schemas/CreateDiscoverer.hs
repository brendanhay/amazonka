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
-- Module      : Network.AWS.Schemas.CreateDiscoverer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a discoverer.
module Network.AWS.Schemas.CreateDiscoverer
  ( -- * Creating a Request
    CreateDiscoverer (..),
    newCreateDiscoverer,

    -- * Request Lenses
    createDiscoverer_crossAccount,
    createDiscoverer_description,
    createDiscoverer_tags,
    createDiscoverer_sourceArn,

    -- * Destructuring the Response
    CreateDiscovererResponse (..),
    newCreateDiscovererResponse,

    -- * Response Lenses
    createDiscovererResponse_state,
    createDiscovererResponse_crossAccount,
    createDiscovererResponse_sourceArn,
    createDiscovererResponse_discovererId,
    createDiscovererResponse_description,
    createDiscovererResponse_tags,
    createDiscovererResponse_discovererArn,
    createDiscovererResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Schemas.Types

-- |
--
-- /See:/ 'newCreateDiscoverer' smart constructor.
data CreateDiscoverer = CreateDiscoverer'
  { -- | Support discovery of schemas in events sent to the bus from another
    -- account. (default: true).
    crossAccount :: Prelude.Maybe Prelude.Bool,
    -- | A description for the discoverer.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'crossAccount', 'createDiscoverer_crossAccount' - Support discovery of schemas in events sent to the bus from another
-- account. (default: true).
--
-- 'description', 'createDiscoverer_description' - A description for the discoverer.
--
-- 'tags', 'createDiscoverer_tags' - Tags associated with the resource.
--
-- 'sourceArn', 'createDiscoverer_sourceArn' - The ARN of the event bus.
newCreateDiscoverer ::
  -- | 'sourceArn'
  Prelude.Text ->
  CreateDiscoverer
newCreateDiscoverer pSourceArn_ =
  CreateDiscoverer'
    { crossAccount = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | Support discovery of schemas in events sent to the bus from another
-- account. (default: true).
createDiscoverer_crossAccount :: Lens.Lens' CreateDiscoverer (Prelude.Maybe Prelude.Bool)
createDiscoverer_crossAccount = Lens.lens (\CreateDiscoverer' {crossAccount} -> crossAccount) (\s@CreateDiscoverer' {} a -> s {crossAccount = a} :: CreateDiscoverer)

-- | A description for the discoverer.
createDiscoverer_description :: Lens.Lens' CreateDiscoverer (Prelude.Maybe Prelude.Text)
createDiscoverer_description = Lens.lens (\CreateDiscoverer' {description} -> description) (\s@CreateDiscoverer' {} a -> s {description = a} :: CreateDiscoverer)

-- | Tags associated with the resource.
createDiscoverer_tags :: Lens.Lens' CreateDiscoverer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDiscoverer_tags = Lens.lens (\CreateDiscoverer' {tags} -> tags) (\s@CreateDiscoverer' {} a -> s {tags = a} :: CreateDiscoverer) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the event bus.
createDiscoverer_sourceArn :: Lens.Lens' CreateDiscoverer Prelude.Text
createDiscoverer_sourceArn = Lens.lens (\CreateDiscoverer' {sourceArn} -> sourceArn) (\s@CreateDiscoverer' {} a -> s {sourceArn = a} :: CreateDiscoverer)

instance Core.AWSRequest CreateDiscoverer where
  type
    AWSResponse CreateDiscoverer =
      CreateDiscovererResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiscovererResponse'
            Prelude.<$> (x Core..?> "State")
            Prelude.<*> (x Core..?> "CrossAccount")
            Prelude.<*> (x Core..?> "SourceArn")
            Prelude.<*> (x Core..?> "DiscovererId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "DiscovererArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDiscoverer

instance Prelude.NFData CreateDiscoverer

instance Core.ToHeaders CreateDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDiscoverer where
  toJSON CreateDiscoverer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CrossAccount" Core..=) Prelude.<$> crossAccount,
            ("Description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("SourceArn" Core..= sourceArn)
          ]
      )

instance Core.ToPath CreateDiscoverer where
  toPath = Prelude.const "/v1/discoverers"

instance Core.ToQuery CreateDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDiscovererResponse' smart constructor.
data CreateDiscovererResponse = CreateDiscovererResponse'
  { -- | The state of the discoverer.
    state :: Prelude.Maybe DiscovererState,
    -- | The Status if the discoverer will discover schemas from events sent from
    -- another account.
    crossAccount :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the event bus.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the discoverer.
    discovererId :: Prelude.Maybe Prelude.Text,
    -- | The description of the discoverer.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'state', 'createDiscovererResponse_state' - The state of the discoverer.
--
-- 'crossAccount', 'createDiscovererResponse_crossAccount' - The Status if the discoverer will discover schemas from events sent from
-- another account.
--
-- 'sourceArn', 'createDiscovererResponse_sourceArn' - The ARN of the event bus.
--
-- 'discovererId', 'createDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'description', 'createDiscovererResponse_description' - The description of the discoverer.
--
-- 'tags', 'createDiscovererResponse_tags' - Tags associated with the resource.
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
    { state = Prelude.Nothing,
      crossAccount = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      discovererId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      discovererArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the discoverer.
createDiscovererResponse_state :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe DiscovererState)
createDiscovererResponse_state = Lens.lens (\CreateDiscovererResponse' {state} -> state) (\s@CreateDiscovererResponse' {} a -> s {state = a} :: CreateDiscovererResponse)

-- | The Status if the discoverer will discover schemas from events sent from
-- another account.
createDiscovererResponse_crossAccount :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Bool)
createDiscovererResponse_crossAccount = Lens.lens (\CreateDiscovererResponse' {crossAccount} -> crossAccount) (\s@CreateDiscovererResponse' {} a -> s {crossAccount = a} :: CreateDiscovererResponse)

-- | The ARN of the event bus.
createDiscovererResponse_sourceArn :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_sourceArn = Lens.lens (\CreateDiscovererResponse' {sourceArn} -> sourceArn) (\s@CreateDiscovererResponse' {} a -> s {sourceArn = a} :: CreateDiscovererResponse)

-- | The ID of the discoverer.
createDiscovererResponse_discovererId :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_discovererId = Lens.lens (\CreateDiscovererResponse' {discovererId} -> discovererId) (\s@CreateDiscovererResponse' {} a -> s {discovererId = a} :: CreateDiscovererResponse)

-- | The description of the discoverer.
createDiscovererResponse_description :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_description = Lens.lens (\CreateDiscovererResponse' {description} -> description) (\s@CreateDiscovererResponse' {} a -> s {description = a} :: CreateDiscovererResponse)

-- | Tags associated with the resource.
createDiscovererResponse_tags :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDiscovererResponse_tags = Lens.lens (\CreateDiscovererResponse' {tags} -> tags) (\s@CreateDiscovererResponse' {} a -> s {tags = a} :: CreateDiscovererResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the discoverer.
createDiscovererResponse_discovererArn :: Lens.Lens' CreateDiscovererResponse (Prelude.Maybe Prelude.Text)
createDiscovererResponse_discovererArn = Lens.lens (\CreateDiscovererResponse' {discovererArn} -> discovererArn) (\s@CreateDiscovererResponse' {} a -> s {discovererArn = a} :: CreateDiscovererResponse)

-- | The response's http status code.
createDiscovererResponse_httpStatus :: Lens.Lens' CreateDiscovererResponse Prelude.Int
createDiscovererResponse_httpStatus = Lens.lens (\CreateDiscovererResponse' {httpStatus} -> httpStatus) (\s@CreateDiscovererResponse' {} a -> s {httpStatus = a} :: CreateDiscovererResponse)

instance Prelude.NFData CreateDiscovererResponse
