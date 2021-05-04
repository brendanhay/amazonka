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
-- Module      : Network.AWS.XRay.Types.BackendConnectionErrors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.BackendConnectionErrors where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newBackendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { otherCount :: Prelude.Maybe Prelude.Int,
    connectionRefusedCount :: Prelude.Maybe Prelude.Int,
    hTTPCode5XXCount :: Prelude.Maybe Prelude.Int,
    timeoutCount :: Prelude.Maybe Prelude.Int,
    unknownHostCount :: Prelude.Maybe Prelude.Int,
    hTTPCode4XXCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'connectionRefusedCount', 'backendConnectionErrors_connectionRefusedCount' -
--
-- 'hTTPCode5XXCount', 'backendConnectionErrors_hTTPCode5XXCount' -
--
-- 'timeoutCount', 'backendConnectionErrors_timeoutCount' -
--
-- 'unknownHostCount', 'backendConnectionErrors_unknownHostCount' -
--
-- 'hTTPCode4XXCount', 'backendConnectionErrors_hTTPCode4XXCount' -
newBackendConnectionErrors ::
  BackendConnectionErrors
newBackendConnectionErrors =
  BackendConnectionErrors'
    { otherCount =
        Prelude.Nothing,
      connectionRefusedCount = Prelude.Nothing,
      hTTPCode5XXCount = Prelude.Nothing,
      timeoutCount = Prelude.Nothing,
      unknownHostCount = Prelude.Nothing,
      hTTPCode4XXCount = Prelude.Nothing
    }

-- |
backendConnectionErrors_otherCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_otherCount = Lens.lens (\BackendConnectionErrors' {otherCount} -> otherCount) (\s@BackendConnectionErrors' {} a -> s {otherCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_connectionRefusedCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_connectionRefusedCount = Lens.lens (\BackendConnectionErrors' {connectionRefusedCount} -> connectionRefusedCount) (\s@BackendConnectionErrors' {} a -> s {connectionRefusedCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_hTTPCode5XXCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_hTTPCode5XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode5XXCount} -> hTTPCode5XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode5XXCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_timeoutCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_timeoutCount = Lens.lens (\BackendConnectionErrors' {timeoutCount} -> timeoutCount) (\s@BackendConnectionErrors' {} a -> s {timeoutCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_unknownHostCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_unknownHostCount = Lens.lens (\BackendConnectionErrors' {unknownHostCount} -> unknownHostCount) (\s@BackendConnectionErrors' {} a -> s {unknownHostCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_hTTPCode4XXCount :: Lens.Lens' BackendConnectionErrors (Prelude.Maybe Prelude.Int)
backendConnectionErrors_hTTPCode4XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode4XXCount} -> hTTPCode4XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode4XXCount = a} :: BackendConnectionErrors)

instance Prelude.Hashable BackendConnectionErrors

instance Prelude.NFData BackendConnectionErrors

instance Prelude.ToJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OtherCount" Prelude..=) Prelude.<$> otherCount,
            ("ConnectionRefusedCount" Prelude..=)
              Prelude.<$> connectionRefusedCount,
            ("HTTPCode5XXCount" Prelude..=)
              Prelude.<$> hTTPCode5XXCount,
            ("TimeoutCount" Prelude..=) Prelude.<$> timeoutCount,
            ("UnknownHostCount" Prelude..=)
              Prelude.<$> unknownHostCount,
            ("HTTPCode4XXCount" Prelude..=)
              Prelude.<$> hTTPCode4XXCount
          ]
      )
