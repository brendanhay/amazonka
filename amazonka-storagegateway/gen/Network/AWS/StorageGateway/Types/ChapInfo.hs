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
-- Module      : Network.AWS.StorageGateway.Types.ChapInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ChapInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
--
-- /See:/ 'newChapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { -- | The iSCSI initiator that connects to the target.
    initiatorName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the volume.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The secret key that the target must provide to participate in mutual
    -- CHAP with the initiator (e.g., Windows client).
    secretToAuthenticateTarget :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The secret key that the initiator (for example, the Windows client) must
    -- provide to participate in mutual CHAP with the target.
    secretToAuthenticateInitiator :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { initiatorName = Prelude.Nothing,
      targetARN = Prelude.Nothing,
      secretToAuthenticateTarget = Prelude.Nothing,
      secretToAuthenticateInitiator = Prelude.Nothing
    }

-- | The iSCSI initiator that connects to the target.
chapInfo_initiatorName :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_initiatorName = Lens.lens (\ChapInfo' {initiatorName} -> initiatorName) (\s@ChapInfo' {} a -> s {initiatorName = a} :: ChapInfo)

-- | The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
chapInfo_targetARN :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_targetARN = Lens.lens (\ChapInfo' {targetARN} -> targetARN) (\s@ChapInfo' {} a -> s {targetARN = a} :: ChapInfo)

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g., Windows client).
chapInfo_secretToAuthenticateTarget :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_secretToAuthenticateTarget = Lens.lens (\ChapInfo' {secretToAuthenticateTarget} -> secretToAuthenticateTarget) (\s@ChapInfo' {} a -> s {secretToAuthenticateTarget = a} :: ChapInfo) Prelude.. Lens.mapping Prelude._Sensitive

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
chapInfo_secretToAuthenticateInitiator :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_secretToAuthenticateInitiator = Lens.lens (\ChapInfo' {secretToAuthenticateInitiator} -> secretToAuthenticateInitiator) (\s@ChapInfo' {} a -> s {secretToAuthenticateInitiator = a} :: ChapInfo) Prelude.. Lens.mapping Prelude._Sensitive

instance Prelude.FromJSON ChapInfo where
  parseJSON =
    Prelude.withObject
      "ChapInfo"
      ( \x ->
          ChapInfo'
            Prelude.<$> (x Prelude..:? "InitiatorName")
            Prelude.<*> (x Prelude..:? "TargetARN")
            Prelude.<*> (x Prelude..:? "SecretToAuthenticateTarget")
            Prelude.<*> (x Prelude..:? "SecretToAuthenticateInitiator")
      )

instance Prelude.Hashable ChapInfo

instance Prelude.NFData ChapInfo
