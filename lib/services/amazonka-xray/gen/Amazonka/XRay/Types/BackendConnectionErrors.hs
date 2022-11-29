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
-- Module      : Amazonka.XRay.Types.BackendConnectionErrors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.BackendConnectionErrors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newBackendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { otherCount :: Prelude.Maybe Prelude.Int,
    unknownHostCount :: Prelude.Maybe Prelude.Int,
    hTTPCode4XXCount :: Prelude.Maybe Prelude.Int,
    connectionRefusedCount :: Prelude.Maybe Prelude.Int,
    timeoutCount :: Prelude.Maybe Prelude.Int,
    hTTPCode5XXCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendConnectionErrors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otherCount', 'backendConnectionErrors_otherCount' -
--
-- 'unknownHostCount', 'backendConnectionErrors_unknownHostCount' -
--
-- 'hTTPCode4XXCount', 'backendConnectionErrors_hTTPCode4XXCount' -
--
-- 'connectionRefusedCount', 'backendConnectionErrors_connectionRefusedCount' -
--
-- 'timeoutCount', 'backendConnectionErrors_timeoutCount' -
--
-- 'hTTPCode5XXCount', 'backendConnectionErrors_hTTPCode5XXCount' -
newBackendConnectionErrors ::
  BackendConnectionErrors
newBackendConnectionErrors =
  BackendConnectionErrors'
    { otherCount =
        Prelude.Nothing,
      unknownHostCount = Prelude.Nothing,
      hTTPCode4XXCount = Prelude.Nothing,
      connectionRefusedCount = Prelude.Nothing,
      timeoutCount = Prelude.Nothing,
      hTTPCode5XXCount = Prelude.Nothing
    }

-- |
backendConnectionErrors_otherCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_otherCount = Lens.lens (\BackendConnectionErrors' {otherCount} -> otherCount) (\s@BackendConnectionErrors' {} a -> s {otherCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_unknownHostCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_unknownHostCount = Lens.lens (\BackendConnectionErrors' {unknownHostCount} -> unknownHostCount) (\s@BackendConnectionErrors' {} a -> s {unknownHostCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_hTTPCode4XXCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_hTTPCode4XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode4XXCount} -> hTTPCode4XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode4XXCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_connectionRefusedCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_connectionRefusedCount = Lens.lens (\BackendConnectionErrors' {connectionRefusedCount} -> connectionRefusedCount) (\s@BackendConnectionErrors' {} a -> s {connectionRefusedCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_timeoutCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_timeoutCount = Lens.lens (\BackendConnectionErrors' {timeoutCount} -> timeoutCount) (\s@BackendConnectionErrors' {} a -> s {timeoutCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_hTTPCode5XXCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_hTTPCode5XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode5XXCount} -> hTTPCode5XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode5XXCount = a} :: BackendConnectionErrors)

instance Prelude.Hashable BackendConnectionErrors where
  hashWithSalt _salt BackendConnectionErrors' {..} =
    _salt `Prelude.hashWithSalt` otherCount
      `Prelude.hashWithSalt` unknownHostCount
      `Prelude.hashWithSalt` hTTPCode4XXCount
      `Prelude.hashWithSalt` connectionRefusedCount
      `Prelude.hashWithSalt` timeoutCount
      `Prelude.hashWithSalt` hTTPCode5XXCount

instance Prelude.NFData BackendConnectionErrors where
  rnf BackendConnectionErrors' {..} =
    Prelude.rnf otherCount
      `Prelude.seq` Prelude.rnf unknownHostCount
      `Prelude.seq` Prelude.rnf hTTPCode4XXCount
      `Prelude.seq` Prelude.rnf connectionRefusedCount
      `Prelude.seq` Prelude.rnf timeoutCount
      `Prelude.seq` Prelude.rnf hTTPCode5XXCount

instance Core.ToJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OtherCount" Core..=) Prelude.<$> otherCount,
            ("UnknownHostCount" Core..=)
              Prelude.<$> unknownHostCount,
            ("HTTPCode4XXCount" Core..=)
              Prelude.<$> hTTPCode4XXCount,
            ("ConnectionRefusedCount" Core..=)
              Prelude.<$> connectionRefusedCount,
            ("TimeoutCount" Core..=) Prelude.<$> timeoutCount,
            ("HTTPCode5XXCount" Core..=)
              Prelude.<$> hTTPCode5XXCount
          ]
      )
