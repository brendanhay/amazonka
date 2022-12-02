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
-- Module      : Amazonka.ImageBuilder.Types.Logging
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Logging where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.S3Logs
import qualified Amazonka.Prelude as Prelude

-- | Logging configuration defines where Image Builder uploads your logs.
--
-- /See:/ 'newLogging' smart constructor.
data Logging = Logging'
  { -- | The Amazon S3 logging configuration.
    s3Logs :: Prelude.Maybe S3Logs
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Logging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'logging_s3Logs' - The Amazon S3 logging configuration.
newLogging ::
  Logging
newLogging = Logging' {s3Logs = Prelude.Nothing}

-- | The Amazon S3 logging configuration.
logging_s3Logs :: Lens.Lens' Logging (Prelude.Maybe S3Logs)
logging_s3Logs = Lens.lens (\Logging' {s3Logs} -> s3Logs) (\s@Logging' {} a -> s {s3Logs = a} :: Logging)

instance Data.FromJSON Logging where
  parseJSON =
    Data.withObject
      "Logging"
      (\x -> Logging' Prelude.<$> (x Data..:? "s3Logs"))

instance Prelude.Hashable Logging where
  hashWithSalt _salt Logging' {..} =
    _salt `Prelude.hashWithSalt` s3Logs

instance Prelude.NFData Logging where
  rnf Logging' {..} = Prelude.rnf s3Logs

instance Data.ToJSON Logging where
  toJSON Logging' {..} =
    Data.object
      ( Prelude.catMaybes
          [("s3Logs" Data..=) Prelude.<$> s3Logs]
      )
