{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.Types.EgressAccessLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.EgressAccessLogs where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configure egress access logging.
--
-- /See:/ 'newEgressAccessLogs' smart constructor.
data EgressAccessLogs = EgressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EgressAccessLogs where
  parseJSON =
    Prelude.withObject
      "EgressAccessLogs"
      ( \x ->
          EgressAccessLogs'
            Prelude.<$> (x Prelude..:? "logGroupName")
      )

instance Prelude.Hashable EgressAccessLogs

instance Prelude.NFData EgressAccessLogs

instance Prelude.ToJSON EgressAccessLogs where
  toJSON EgressAccessLogs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("logGroupName" Prelude..=)
              Prelude.<$> logGroupName
          ]
      )
