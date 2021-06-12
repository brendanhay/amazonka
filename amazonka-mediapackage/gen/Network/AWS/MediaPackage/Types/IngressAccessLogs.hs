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
-- Module      : Network.AWS.MediaPackage.Types.IngressAccessLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.IngressAccessLogs where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configure ingress access logging.
--
-- /See:/ 'newIngressAccessLogs' smart constructor.
data IngressAccessLogs = IngressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IngressAccessLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'ingressAccessLogs_logGroupName' - Customize the log group name.
newIngressAccessLogs ::
  IngressAccessLogs
newIngressAccessLogs =
  IngressAccessLogs' {logGroupName = Core.Nothing}

-- | Customize the log group name.
ingressAccessLogs_logGroupName :: Lens.Lens' IngressAccessLogs (Core.Maybe Core.Text)
ingressAccessLogs_logGroupName = Lens.lens (\IngressAccessLogs' {logGroupName} -> logGroupName) (\s@IngressAccessLogs' {} a -> s {logGroupName = a} :: IngressAccessLogs)

instance Core.FromJSON IngressAccessLogs where
  parseJSON =
    Core.withObject
      "IngressAccessLogs"
      ( \x ->
          IngressAccessLogs'
            Core.<$> (x Core..:? "logGroupName")
      )

instance Core.Hashable IngressAccessLogs

instance Core.NFData IngressAccessLogs

instance Core.ToJSON IngressAccessLogs where
  toJSON IngressAccessLogs' {..} =
    Core.object
      ( Core.catMaybes
          [("logGroupName" Core..=) Core.<$> logGroupName]
      )
