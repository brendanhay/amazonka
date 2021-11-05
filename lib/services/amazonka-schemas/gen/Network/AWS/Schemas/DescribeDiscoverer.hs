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
-- Module      : Network.AWS.Schemas.DescribeDiscoverer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the discoverer.
module Network.AWS.Schemas.DescribeDiscoverer
  ( -- * Creating a Request
    DescribeDiscoverer (..),
    newDescribeDiscoverer,

    -- * Request Lenses
    describeDiscoverer_discovererId,

    -- * Destructuring the Response
    DescribeDiscovererResponse (..),
    newDescribeDiscovererResponse,

    -- * Response Lenses
    describeDiscovererResponse_state,
    describeDiscovererResponse_crossAccount,
    describeDiscovererResponse_sourceArn,
    describeDiscovererResponse_discovererId,
    describeDiscovererResponse_description,
    describeDiscovererResponse_tags,
    describeDiscovererResponse_discovererArn,
    describeDiscovererResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Schemas.Types

-- | /See:/ 'newDescribeDiscoverer' smart constructor.
data DescribeDiscoverer = DescribeDiscoverer'
  { -- | The ID of the discoverer.
    discovererId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDiscoverer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discovererId', 'describeDiscoverer_discovererId' - The ID of the discoverer.
newDescribeDiscoverer ::
  -- | 'discovererId'
  Prelude.Text ->
  DescribeDiscoverer
newDescribeDiscoverer pDiscovererId_ =
  DescribeDiscoverer' {discovererId = pDiscovererId_}

-- | The ID of the discoverer.
describeDiscoverer_discovererId :: Lens.Lens' DescribeDiscoverer Prelude.Text
describeDiscoverer_discovererId = Lens.lens (\DescribeDiscoverer' {discovererId} -> discovererId) (\s@DescribeDiscoverer' {} a -> s {discovererId = a} :: DescribeDiscoverer)

instance Core.AWSRequest DescribeDiscoverer where
  type
    AWSResponse DescribeDiscoverer =
      DescribeDiscovererResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDiscovererResponse'
            Prelude.<$> (x Core..?> "State")
            Prelude.<*> (x Core..?> "CrossAccount")
            Prelude.<*> (x Core..?> "SourceArn")
            Prelude.<*> (x Core..?> "DiscovererId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "DiscovererArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDiscoverer

instance Prelude.NFData DescribeDiscoverer

instance Core.ToHeaders DescribeDiscoverer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeDiscoverer where
  toPath DescribeDiscoverer' {..} =
    Prelude.mconcat
      ["/v1/discoverers/id/", Core.toBS discovererId]

instance Core.ToQuery DescribeDiscoverer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDiscovererResponse' smart constructor.
data DescribeDiscovererResponse = DescribeDiscovererResponse'
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
-- Create a value of 'DescribeDiscovererResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'describeDiscovererResponse_state' - The state of the discoverer.
--
-- 'crossAccount', 'describeDiscovererResponse_crossAccount' - The Status if the discoverer will discover schemas from events sent from
-- another account.
--
-- 'sourceArn', 'describeDiscovererResponse_sourceArn' - The ARN of the event bus.
--
-- 'discovererId', 'describeDiscovererResponse_discovererId' - The ID of the discoverer.
--
-- 'description', 'describeDiscovererResponse_description' - The description of the discoverer.
--
-- 'tags', 'describeDiscovererResponse_tags' - Tags associated with the resource.
--
-- 'discovererArn', 'describeDiscovererResponse_discovererArn' - The ARN of the discoverer.
--
-- 'httpStatus', 'describeDiscovererResponse_httpStatus' - The response's http status code.
newDescribeDiscovererResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDiscovererResponse
newDescribeDiscovererResponse pHttpStatus_ =
  DescribeDiscovererResponse'
    { state =
        Prelude.Nothing,
      crossAccount = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      discovererId = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      discovererArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the discoverer.
describeDiscovererResponse_state :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe DiscovererState)
describeDiscovererResponse_state = Lens.lens (\DescribeDiscovererResponse' {state} -> state) (\s@DescribeDiscovererResponse' {} a -> s {state = a} :: DescribeDiscovererResponse)

-- | The Status if the discoverer will discover schemas from events sent from
-- another account.
describeDiscovererResponse_crossAccount :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe Prelude.Bool)
describeDiscovererResponse_crossAccount = Lens.lens (\DescribeDiscovererResponse' {crossAccount} -> crossAccount) (\s@DescribeDiscovererResponse' {} a -> s {crossAccount = a} :: DescribeDiscovererResponse)

-- | The ARN of the event bus.
describeDiscovererResponse_sourceArn :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe Prelude.Text)
describeDiscovererResponse_sourceArn = Lens.lens (\DescribeDiscovererResponse' {sourceArn} -> sourceArn) (\s@DescribeDiscovererResponse' {} a -> s {sourceArn = a} :: DescribeDiscovererResponse)

-- | The ID of the discoverer.
describeDiscovererResponse_discovererId :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe Prelude.Text)
describeDiscovererResponse_discovererId = Lens.lens (\DescribeDiscovererResponse' {discovererId} -> discovererId) (\s@DescribeDiscovererResponse' {} a -> s {discovererId = a} :: DescribeDiscovererResponse)

-- | The description of the discoverer.
describeDiscovererResponse_description :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe Prelude.Text)
describeDiscovererResponse_description = Lens.lens (\DescribeDiscovererResponse' {description} -> description) (\s@DescribeDiscovererResponse' {} a -> s {description = a} :: DescribeDiscovererResponse)

-- | Tags associated with the resource.
describeDiscovererResponse_tags :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeDiscovererResponse_tags = Lens.lens (\DescribeDiscovererResponse' {tags} -> tags) (\s@DescribeDiscovererResponse' {} a -> s {tags = a} :: DescribeDiscovererResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the discoverer.
describeDiscovererResponse_discovererArn :: Lens.Lens' DescribeDiscovererResponse (Prelude.Maybe Prelude.Text)
describeDiscovererResponse_discovererArn = Lens.lens (\DescribeDiscovererResponse' {discovererArn} -> discovererArn) (\s@DescribeDiscovererResponse' {} a -> s {discovererArn = a} :: DescribeDiscovererResponse)

-- | The response's http status code.
describeDiscovererResponse_httpStatus :: Lens.Lens' DescribeDiscovererResponse Prelude.Int
describeDiscovererResponse_httpStatus = Lens.lens (\DescribeDiscovererResponse' {httpStatus} -> httpStatus) (\s@DescribeDiscovererResponse' {} a -> s {httpStatus = a} :: DescribeDiscovererResponse)

instance Prelude.NFData DescribeDiscovererResponse
