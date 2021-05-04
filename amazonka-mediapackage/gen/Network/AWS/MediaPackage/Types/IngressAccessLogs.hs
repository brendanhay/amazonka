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
-- Module      : Network.AWS.MediaPackage.Types.IngressAccessLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.IngressAccessLogs where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configure ingress access logging.
--
-- /See:/ 'newIngressAccessLogs' smart constructor.
data IngressAccessLogs = IngressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON IngressAccessLogs where
  parseJSON =
    Prelude.withObject
      "IngressAccessLogs"
      ( \x ->
          IngressAccessLogs'
            Prelude.<$> (x Prelude..:? "logGroupName")
      )

instance Prelude.Hashable IngressAccessLogs

instance Prelude.NFData IngressAccessLogs

instance Prelude.ToJSON IngressAccessLogs where
  toJSON IngressAccessLogs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("logGroupName" Prelude..=)
              Prelude.<$> logGroupName
          ]
      )
