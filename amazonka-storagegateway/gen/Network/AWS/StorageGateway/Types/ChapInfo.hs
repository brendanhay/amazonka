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
-- Module      : Network.AWS.StorageGateway.Types.ChapInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ChapInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
--
-- /See:/ 'newChapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { -- | The iSCSI initiator that connects to the target.
    initiatorName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the volume.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    targetARN :: Core.Maybe Core.Text,
    -- | The secret key that the target must provide to participate in mutual
    -- CHAP with the initiator (e.g., Windows client).
    secretToAuthenticateTarget :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The secret key that the initiator (for example, the Windows client) must
    -- provide to participate in mutual CHAP with the target.
    secretToAuthenticateInitiator :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChapInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initiatorName', 'chapInfo_initiatorName' - The iSCSI initiator that connects to the target.
--
-- 'targetARN', 'chapInfo_targetARN' - The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
--
-- 'secretToAuthenticateTarget', 'chapInfo_secretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g., Windows client).
--
-- 'secretToAuthenticateInitiator', 'chapInfo_secretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
newChapInfo ::
  ChapInfo
newChapInfo =
  ChapInfo'
    { initiatorName = Core.Nothing,
      targetARN = Core.Nothing,
      secretToAuthenticateTarget = Core.Nothing,
      secretToAuthenticateInitiator = Core.Nothing
    }

-- | The iSCSI initiator that connects to the target.
chapInfo_initiatorName :: Lens.Lens' ChapInfo (Core.Maybe Core.Text)
chapInfo_initiatorName = Lens.lens (\ChapInfo' {initiatorName} -> initiatorName) (\s@ChapInfo' {} a -> s {initiatorName = a} :: ChapInfo)

-- | The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
chapInfo_targetARN :: Lens.Lens' ChapInfo (Core.Maybe Core.Text)
chapInfo_targetARN = Lens.lens (\ChapInfo' {targetARN} -> targetARN) (\s@ChapInfo' {} a -> s {targetARN = a} :: ChapInfo)

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g., Windows client).
chapInfo_secretToAuthenticateTarget :: Lens.Lens' ChapInfo (Core.Maybe Core.Text)
chapInfo_secretToAuthenticateTarget = Lens.lens (\ChapInfo' {secretToAuthenticateTarget} -> secretToAuthenticateTarget) (\s@ChapInfo' {} a -> s {secretToAuthenticateTarget = a} :: ChapInfo) Core.. Lens.mapping Core._Sensitive

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
chapInfo_secretToAuthenticateInitiator :: Lens.Lens' ChapInfo (Core.Maybe Core.Text)
chapInfo_secretToAuthenticateInitiator = Lens.lens (\ChapInfo' {secretToAuthenticateInitiator} -> secretToAuthenticateInitiator) (\s@ChapInfo' {} a -> s {secretToAuthenticateInitiator = a} :: ChapInfo) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON ChapInfo where
  parseJSON =
    Core.withObject
      "ChapInfo"
      ( \x ->
          ChapInfo'
            Core.<$> (x Core..:? "InitiatorName")
            Core.<*> (x Core..:? "TargetARN")
            Core.<*> (x Core..:? "SecretToAuthenticateTarget")
            Core.<*> (x Core..:? "SecretToAuthenticateInitiator")
      )

instance Core.Hashable ChapInfo

instance Core.NFData ChapInfo
