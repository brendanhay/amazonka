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
-- Module      : Network.AWS.AppMesh.Types.Logging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.Logging where

import Network.AWS.AppMesh.Types.AccessLog
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON Logging where
  parseJSON =
    Core.withObject
      "Logging"
      ( \x ->
          Logging' Prelude.<$> (x Core..:? "accessLog")
      )

instance Prelude.Hashable Logging

instance Prelude.NFData Logging

instance Core.ToJSON Logging where
  toJSON Logging' {..} =
    Core.object
      ( Prelude.catMaybes
          [("accessLog" Core..=) Prelude.<$> accessLog]
      )
