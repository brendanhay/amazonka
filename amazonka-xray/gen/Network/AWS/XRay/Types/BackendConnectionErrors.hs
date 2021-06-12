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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- |
--
-- /See:/ 'newBackendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { otherCount :: Core.Maybe Core.Int,
    connectionRefusedCount :: Core.Maybe Core.Int,
    hTTPCode5XXCount :: Core.Maybe Core.Int,
    timeoutCount :: Core.Maybe Core.Int,
    unknownHostCount :: Core.Maybe Core.Int,
    hTTPCode4XXCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { otherCount = Core.Nothing,
      connectionRefusedCount = Core.Nothing,
      hTTPCode5XXCount = Core.Nothing,
      timeoutCount = Core.Nothing,
      unknownHostCount = Core.Nothing,
      hTTPCode4XXCount = Core.Nothing
    }

-- |
backendConnectionErrors_otherCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
backendConnectionErrors_otherCount = Lens.lens (\BackendConnectionErrors' {otherCount} -> otherCount) (\s@BackendConnectionErrors' {} a -> s {otherCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_connectionRefusedCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
backendConnectionErrors_connectionRefusedCount = Lens.lens (\BackendConnectionErrors' {connectionRefusedCount} -> connectionRefusedCount) (\s@BackendConnectionErrors' {} a -> s {connectionRefusedCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_hTTPCode5XXCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
backendConnectionErrors_hTTPCode5XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode5XXCount} -> hTTPCode5XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode5XXCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_timeoutCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
backendConnectionErrors_timeoutCount = Lens.lens (\BackendConnectionErrors' {timeoutCount} -> timeoutCount) (\s@BackendConnectionErrors' {} a -> s {timeoutCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_unknownHostCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
backendConnectionErrors_unknownHostCount = Lens.lens (\BackendConnectionErrors' {unknownHostCount} -> unknownHostCount) (\s@BackendConnectionErrors' {} a -> s {unknownHostCount = a} :: BackendConnectionErrors)

-- |
backendConnectionErrors_hTTPCode4XXCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
backendConnectionErrors_hTTPCode4XXCount = Lens.lens (\BackendConnectionErrors' {hTTPCode4XXCount} -> hTTPCode4XXCount) (\s@BackendConnectionErrors' {} a -> s {hTTPCode4XXCount = a} :: BackendConnectionErrors)

instance Core.Hashable BackendConnectionErrors

instance Core.NFData BackendConnectionErrors

instance Core.ToJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OtherCount" Core..=) Core.<$> otherCount,
            ("ConnectionRefusedCount" Core..=)
              Core.<$> connectionRefusedCount,
            ("HTTPCode5XXCount" Core..=)
              Core.<$> hTTPCode5XXCount,
            ("TimeoutCount" Core..=) Core.<$> timeoutCount,
            ("UnknownHostCount" Core..=)
              Core.<$> unknownHostCount,
            ("HTTPCode4XXCount" Core..=)
              Core.<$> hTTPCode4XXCount
          ]
      )
