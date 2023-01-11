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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.StagingSourceServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Source server in staging account that extended source server connected
-- to.
--
-- /See:/ 'newStagingSourceServer' smart constructor.
data StagingSourceServer = StagingSourceServer'
  { -- | The ARN of the source server.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Hostname of staging source server.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | A list of tags associated with the staging source server.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text))
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
-- 'arn', 'stagingSourceServer_arn' - The ARN of the source server.
--
-- 'hostname', 'stagingSourceServer_hostname' - Hostname of staging source server.
--
-- 'tags', 'stagingSourceServer_tags' - A list of tags associated with the staging source server.
newStagingSourceServer ::
  StagingSourceServer
newStagingSourceServer =
  StagingSourceServer'
    { arn = Prelude.Nothing,
      hostname = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of the source server.
stagingSourceServer_arn :: Lens.Lens' StagingSourceServer (Prelude.Maybe Prelude.Text)
stagingSourceServer_arn = Lens.lens (\StagingSourceServer' {arn} -> arn) (\s@StagingSourceServer' {} a -> s {arn = a} :: StagingSourceServer)

-- | Hostname of staging source server.
stagingSourceServer_hostname :: Lens.Lens' StagingSourceServer (Prelude.Maybe Prelude.Text)
stagingSourceServer_hostname = Lens.lens (\StagingSourceServer' {hostname} -> hostname) (\s@StagingSourceServer' {} a -> s {hostname = a} :: StagingSourceServer)

-- | A list of tags associated with the staging source server.
stagingSourceServer_tags :: Lens.Lens' StagingSourceServer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stagingSourceServer_tags = Lens.lens (\StagingSourceServer' {tags} -> tags) (\s@StagingSourceServer' {} a -> s {tags = a} :: StagingSourceServer) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON StagingSourceServer where
  parseJSON =
    Data.withObject
      "StagingSourceServer"
      ( \x ->
          StagingSourceServer'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "hostname")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StagingSourceServer where
  hashWithSalt _salt StagingSourceServer' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` tags

instance Prelude.NFData StagingSourceServer where
  rnf StagingSourceServer' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf tags
