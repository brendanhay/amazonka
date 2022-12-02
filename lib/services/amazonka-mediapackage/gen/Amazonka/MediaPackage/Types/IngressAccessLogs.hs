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
-- Module      : Amazonka.MediaPackage.Types.IngressAccessLogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.IngressAccessLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configure ingress access logging.
--
-- /See:/ 'newIngressAccessLogs' smart constructor.
data IngressAccessLogs = IngressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  IngressAccessLogs' {logGroupName = Prelude.Nothing}

-- | Customize the log group name.
ingressAccessLogs_logGroupName :: Lens.Lens' IngressAccessLogs (Prelude.Maybe Prelude.Text)
ingressAccessLogs_logGroupName = Lens.lens (\IngressAccessLogs' {logGroupName} -> logGroupName) (\s@IngressAccessLogs' {} a -> s {logGroupName = a} :: IngressAccessLogs)

instance Data.FromJSON IngressAccessLogs where
  parseJSON =
    Data.withObject
      "IngressAccessLogs"
      ( \x ->
          IngressAccessLogs'
            Prelude.<$> (x Data..:? "logGroupName")
      )

instance Prelude.Hashable IngressAccessLogs where
  hashWithSalt _salt IngressAccessLogs' {..} =
    _salt `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData IngressAccessLogs where
  rnf IngressAccessLogs' {..} = Prelude.rnf logGroupName

instance Data.ToJSON IngressAccessLogs where
  toJSON IngressAccessLogs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("logGroupName" Data..=) Prelude.<$> logGroupName]
      )
