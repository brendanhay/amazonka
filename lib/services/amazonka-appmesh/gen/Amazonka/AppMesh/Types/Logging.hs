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
-- Module      : Amazonka.AppMesh.Types.Logging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.Logging where

import Amazonka.AppMesh.Types.AccessLog
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the logging information for a virtual node.
--
-- /See:/ 'newLogging' smart constructor.
data Logging = Logging'
  { -- | The access log configuration for a virtual node.
    accessLog :: Prelude.Maybe AccessLog
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
-- 'accessLog', 'logging_accessLog' - The access log configuration for a virtual node.
newLogging ::
  Logging
newLogging = Logging' {accessLog = Prelude.Nothing}

-- | The access log configuration for a virtual node.
logging_accessLog :: Lens.Lens' Logging (Prelude.Maybe AccessLog)
logging_accessLog = Lens.lens (\Logging' {accessLog} -> accessLog) (\s@Logging' {} a -> s {accessLog = a} :: Logging)

instance Data.FromJSON Logging where
  parseJSON =
    Data.withObject
      "Logging"
      ( \x ->
          Logging' Prelude.<$> (x Data..:? "accessLog")
      )

instance Prelude.Hashable Logging where
  hashWithSalt _salt Logging' {..} =
    _salt `Prelude.hashWithSalt` accessLog

instance Prelude.NFData Logging where
  rnf Logging' {..} = Prelude.rnf accessLog

instance Data.ToJSON Logging where
  toJSON Logging' {..} =
    Data.object
      ( Prelude.catMaybes
          [("accessLog" Data..=) Prelude.<$> accessLog]
      )
