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
-- Module      : Amazonka.DrS.Types.StagingSourceServer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.StagingSourceServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Source server in staging account that extended source server connected
-- to.
--
-- /See:/ 'newStagingSourceServer' smart constructor.
data StagingSourceServer = StagingSourceServer'
  { -- | A list of tags associated with the staging source server.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The ARN of the source server.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Hostname of staging source server.
    hostname :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StagingSourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'stagingSourceServer_tags' - A list of tags associated with the staging source server.
--
-- 'arn', 'stagingSourceServer_arn' - The ARN of the source server.
--
-- 'hostname', 'stagingSourceServer_hostname' - Hostname of staging source server.
newStagingSourceServer ::
  StagingSourceServer
newStagingSourceServer =
  StagingSourceServer'
    { tags = Prelude.Nothing,
      arn = Prelude.Nothing,
      hostname = Prelude.Nothing
    }

-- | A list of tags associated with the staging source server.
stagingSourceServer_tags :: Lens.Lens' StagingSourceServer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stagingSourceServer_tags = Lens.lens (\StagingSourceServer' {tags} -> tags) (\s@StagingSourceServer' {} a -> s {tags = a} :: StagingSourceServer) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The ARN of the source server.
stagingSourceServer_arn :: Lens.Lens' StagingSourceServer (Prelude.Maybe Prelude.Text)
stagingSourceServer_arn = Lens.lens (\StagingSourceServer' {arn} -> arn) (\s@StagingSourceServer' {} a -> s {arn = a} :: StagingSourceServer)

-- | Hostname of staging source server.
stagingSourceServer_hostname :: Lens.Lens' StagingSourceServer (Prelude.Maybe Prelude.Text)
stagingSourceServer_hostname = Lens.lens (\StagingSourceServer' {hostname} -> hostname) (\s@StagingSourceServer' {} a -> s {hostname = a} :: StagingSourceServer)

instance Core.FromJSON StagingSourceServer where
  parseJSON =
    Core.withObject
      "StagingSourceServer"
      ( \x ->
          StagingSourceServer'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "hostname")
      )

instance Prelude.Hashable StagingSourceServer where
  hashWithSalt _salt StagingSourceServer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` hostname

instance Prelude.NFData StagingSourceServer where
  rnf StagingSourceServer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf hostname
