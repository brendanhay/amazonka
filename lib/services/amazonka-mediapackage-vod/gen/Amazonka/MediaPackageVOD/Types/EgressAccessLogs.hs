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
-- Module      : Amazonka.MediaPackageVOD.Types.EgressAccessLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.EgressAccessLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configure egress access logging.
--
-- /See:/ 'newEgressAccessLogs' smart constructor.
data EgressAccessLogs = EgressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EgressAccessLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'egressAccessLogs_logGroupName' - Customize the log group name.
newEgressAccessLogs ::
  EgressAccessLogs
newEgressAccessLogs =
  EgressAccessLogs' {logGroupName = Prelude.Nothing}

-- | Customize the log group name.
egressAccessLogs_logGroupName :: Lens.Lens' EgressAccessLogs (Prelude.Maybe Prelude.Text)
egressAccessLogs_logGroupName = Lens.lens (\EgressAccessLogs' {logGroupName} -> logGroupName) (\s@EgressAccessLogs' {} a -> s {logGroupName = a} :: EgressAccessLogs)

instance Data.FromJSON EgressAccessLogs where
  parseJSON =
    Data.withObject
      "EgressAccessLogs"
      ( \x ->
          EgressAccessLogs'
            Prelude.<$> (x Data..:? "logGroupName")
      )

instance Prelude.Hashable EgressAccessLogs where
  hashWithSalt _salt EgressAccessLogs' {..} =
    _salt `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData EgressAccessLogs where
  rnf EgressAccessLogs' {..} = Prelude.rnf logGroupName

instance Data.ToJSON EgressAccessLogs where
  toJSON EgressAccessLogs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("logGroupName" Data..=) Prelude.<$> logGroupName]
      )
