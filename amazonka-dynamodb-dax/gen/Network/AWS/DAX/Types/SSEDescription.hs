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
-- Module      : Network.AWS.DAX.Types.SSEDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SSEDescription where

import Network.AWS.DAX.Types.SSEStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The description of the server-side encryption status on the specified
-- DAX cluster.
--
-- /See:/ 'newSSEDescription' smart constructor.
data SSEDescription = SSEDescription'
  { -- | The current state of server-side encryption:
    --
    -- -   @ENABLING@ - Server-side encryption is being enabled.
    --
    -- -   @ENABLED@ - Server-side encryption is enabled.
    --
    -- -   @DISABLING@ - Server-side encryption is being disabled.
    --
    -- -   @DISABLED@ - Server-side encryption is disabled.
    status :: Prelude.Maybe SSEStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SSEDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'sSEDescription_status' - The current state of server-side encryption:
--
-- -   @ENABLING@ - Server-side encryption is being enabled.
--
-- -   @ENABLED@ - Server-side encryption is enabled.
--
-- -   @DISABLING@ - Server-side encryption is being disabled.
--
-- -   @DISABLED@ - Server-side encryption is disabled.
newSSEDescription ::
  SSEDescription
newSSEDescription =
  SSEDescription' {status = Prelude.Nothing}

-- | The current state of server-side encryption:
--
-- -   @ENABLING@ - Server-side encryption is being enabled.
--
-- -   @ENABLED@ - Server-side encryption is enabled.
--
-- -   @DISABLING@ - Server-side encryption is being disabled.
--
-- -   @DISABLED@ - Server-side encryption is disabled.
sSEDescription_status :: Lens.Lens' SSEDescription (Prelude.Maybe SSEStatus)
sSEDescription_status = Lens.lens (\SSEDescription' {status} -> status) (\s@SSEDescription' {} a -> s {status = a} :: SSEDescription)

instance Prelude.FromJSON SSEDescription where
  parseJSON =
    Prelude.withObject
      "SSEDescription"
      ( \x ->
          SSEDescription' Prelude.<$> (x Prelude..:? "Status")
      )

instance Prelude.Hashable SSEDescription

instance Prelude.NFData SSEDescription
