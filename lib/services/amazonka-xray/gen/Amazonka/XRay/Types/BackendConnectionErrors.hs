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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.BackendConnectionErrors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newBackendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { connectionRefusedCount :: Prelude.Maybe Prelude.Int,
    hTTPCode4XXCount :: Prelude.Maybe Prelude.Int,
    hTTPCode5XXCount :: Prelude.Maybe Prelude.Int,
    otherCount :: Prelude.Maybe Prelude.Int,
    timeoutCount :: Prelude.Maybe Prelude.Int,
    unknownHostCount :: Prelude.Maybe Prelude.Int
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
-- 'connectionRefusedCount', 'backendConnectionErrors_connectionRefusedCount' -
--
-- 'hTTPCode4XXCount', 'backendConnectionErrors_hTTPCode4XXCount' -
--
-- 'hTTPCode5XXCount', 'backendConnectionErrors_hTTPCode5XXCount' -
--
-- 'otherCount', 'backendConnectionErrors_otherCount' -
--
-- 'timeoutCount', 'backendConnectionErrors_timeoutCount' -
--
-- 'unknownHostCount', 'backendConnectionErrors_unknownHostCount' -
newBackendConnectionErrors ::
  BackendConnectionErrors
newBackendConnectionErrors =
  BackendConnectionErrors'
    { connectionRefusedCount =
        Prelude.Nothing,
      hTTPCode4XXCount = Prelude.Nothing,
      hTTPCode5XXCount = Prelude.Nothing,
      otherCount = Prelude.Nothing,
      timeoutCount = Prelude.Nothing,
      unknownHostCount = Prelude.Nothing
    }

backendConnectionErrors_connectionRefusedCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_connectionRefusedCount = Lens.lens (\BackendConnectionErrors' {connectionRefusedCount} -> connectionRefusedCount) (\s@BackendConnectionErrors' {} a -> s {connectionRefusedCount = a} :: BackendConnectionErrors)

backendConnectionErrors_hTTPCode4XXCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_hTTPCode4XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode4XXCount} -> hTTPCode4XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode4XXCount = a} :: BackendConnectionErrors)

backendConnectionErrors_hTTPCode5XXCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_hTTPCode5XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode5XXCount} -> hTTPCode5XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode5XXCount = a} :: BackendConnectionErrors)

backendConnectionErrors_otherCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_otherCount = Lens.lens (\BackendConnectionErrors' {otherCount} -> otherCount) (\s@BackendConnectionErrors' {} a -> s {otherCount = a} :: BackendConnectionErrors)

backendConnectionErrors_timeoutCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_timeoutCount = Lens.lens (\BackendConnectionErrors' {timeoutCount} -> timeoutCount) (\s@BackendConnectionErrors' {} a -> s {timeoutCount = a} :: BackendConnectionErrors)

backendConnectionErrors_unknownHostCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_unknownHostCount = Lens.lens (\BackendConnectionErrors' {unknownHostCount} -> unknownHostCount) (\s@BackendConnectionErrors' {} a -> s {unknownHostCount = a} :: BackendConnectionErrors)

instance Prelude.Hashable BackendConnectionErrors where
  hashWithSalt _salt BackendConnectionErrors' {..} =
    _salt
      `Prelude.hashWithSalt` connectionRefusedCount
      `Prelude.hashWithSalt` hTTPCode4XXCount
      `Prelude.hashWithSalt` hTTPCode5XXCount
      `Prelude.hashWithSalt` otherCount
      `Prelude.hashWithSalt` timeoutCount
      `Prelude.hashWithSalt` unknownHostCount

instance Prelude.NFData BackendConnectionErrors where
  rnf BackendConnectionErrors' {..} =
    Prelude.rnf connectionRefusedCount
      `Prelude.seq` Prelude.rnf hTTPCode4XXCount
      `Prelude.seq` Prelude.rnf hTTPCode5XXCount
      `Prelude.seq` Prelude.rnf otherCount
      `Prelude.seq` Prelude.rnf timeoutCount
      `Prelude.seq` Prelude.rnf unknownHostCount

instance Data.ToJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionRefusedCount" Data..=)
              Prelude.<$> connectionRefusedCount,
            ("HTTPCode4XXCount" Data..=)
              Prelude.<$> hTTPCode4XXCount,
            ("HTTPCode5XXCount" Data..=)
              Prelude.<$> hTTPCode5XXCount,
            ("OtherCount" Data..=) Prelude.<$> otherCount,
            ("TimeoutCount" Data..=) Prelude.<$> timeoutCount,
            ("UnknownHostCount" Data..=)
              Prelude.<$> unknownHostCount
          ]
      )
