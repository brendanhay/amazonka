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
-- Module      : Amazonka.MediaTailor.Types.LogConfigurationForChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.LogConfigurationForChannel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | The log configuration for the channel.
--
-- /See:/ 'newLogConfigurationForChannel' smart constructor.
data LogConfigurationForChannel = LogConfigurationForChannel'
  { -- | The log types.
    logTypes :: Prelude.Maybe [LogType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfigurationForChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logTypes', 'logConfigurationForChannel_logTypes' - The log types.
newLogConfigurationForChannel ::
  LogConfigurationForChannel
newLogConfigurationForChannel =
  LogConfigurationForChannel'
    { logTypes =
        Prelude.Nothing
    }

-- | The log types.
logConfigurationForChannel_logTypes :: Lens.Lens' LogConfigurationForChannel (Prelude.Maybe [LogType])
logConfigurationForChannel_logTypes = Lens.lens (\LogConfigurationForChannel' {logTypes} -> logTypes) (\s@LogConfigurationForChannel' {} a -> s {logTypes = a} :: LogConfigurationForChannel) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LogConfigurationForChannel where
  parseJSON =
    Data.withObject
      "LogConfigurationForChannel"
      ( \x ->
          LogConfigurationForChannel'
            Prelude.<$> (x Data..:? "LogTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LogConfigurationForChannel where
  hashWithSalt _salt LogConfigurationForChannel' {..} =
    _salt `Prelude.hashWithSalt` logTypes

instance Prelude.NFData LogConfigurationForChannel where
  rnf LogConfigurationForChannel' {..} =
    Prelude.rnf logTypes
