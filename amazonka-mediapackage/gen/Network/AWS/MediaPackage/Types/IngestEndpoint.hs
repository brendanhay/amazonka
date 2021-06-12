{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.IngestEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.IngestEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An endpoint for ingesting source content for a Channel.
--
-- /See:/ 'newIngestEndpoint' smart constructor.
data IngestEndpoint = IngestEndpoint'
  { -- | The system generated unique identifier for the IngestEndpoint
    id :: Core.Maybe Core.Text,
    -- | The system generated password for ingest authentication.
    password :: Core.Maybe Core.Text,
    -- | The system generated username for ingest authentication.
    username :: Core.Maybe Core.Text,
    -- | The ingest URL to which the source stream should be sent.
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IngestEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'ingestEndpoint_id' - The system generated unique identifier for the IngestEndpoint
--
-- 'password', 'ingestEndpoint_password' - The system generated password for ingest authentication.
--
-- 'username', 'ingestEndpoint_username' - The system generated username for ingest authentication.
--
-- 'url', 'ingestEndpoint_url' - The ingest URL to which the source stream should be sent.
newIngestEndpoint ::
  IngestEndpoint
newIngestEndpoint =
  IngestEndpoint'
    { id = Core.Nothing,
      password = Core.Nothing,
      username = Core.Nothing,
      url = Core.Nothing
    }

-- | The system generated unique identifier for the IngestEndpoint
ingestEndpoint_id :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ingestEndpoint_id = Lens.lens (\IngestEndpoint' {id} -> id) (\s@IngestEndpoint' {} a -> s {id = a} :: IngestEndpoint)

-- | The system generated password for ingest authentication.
ingestEndpoint_password :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ingestEndpoint_password = Lens.lens (\IngestEndpoint' {password} -> password) (\s@IngestEndpoint' {} a -> s {password = a} :: IngestEndpoint)

-- | The system generated username for ingest authentication.
ingestEndpoint_username :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ingestEndpoint_username = Lens.lens (\IngestEndpoint' {username} -> username) (\s@IngestEndpoint' {} a -> s {username = a} :: IngestEndpoint)

-- | The ingest URL to which the source stream should be sent.
ingestEndpoint_url :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ingestEndpoint_url = Lens.lens (\IngestEndpoint' {url} -> url) (\s@IngestEndpoint' {} a -> s {url = a} :: IngestEndpoint)

instance Core.FromJSON IngestEndpoint where
  parseJSON =
    Core.withObject
      "IngestEndpoint"
      ( \x ->
          IngestEndpoint'
            Core.<$> (x Core..:? "id")
            Core.<*> (x Core..:? "password")
            Core.<*> (x Core..:? "username")
            Core.<*> (x Core..:? "url")
      )

instance Core.Hashable IngestEndpoint

instance Core.NFData IngestEndpoint
