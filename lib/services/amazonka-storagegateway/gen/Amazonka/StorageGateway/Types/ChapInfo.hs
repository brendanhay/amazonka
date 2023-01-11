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
-- Module      : Amazonka.StorageGateway.Types.ChapInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.ChapInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
--
-- /See:/ 'newChapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { -- | The iSCSI initiator that connects to the target.
    initiatorName :: Prelude.Maybe Prelude.Text,
    -- | The secret key that the initiator (for example, the Windows client) must
    -- provide to participate in mutual CHAP with the target.
    secretToAuthenticateInitiator :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The secret key that the target must provide to participate in mutual
    -- CHAP with the initiator (e.g., Windows client).
    secretToAuthenticateTarget :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the volume.
    --
    -- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
    -- hyphens (-).
    targetARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'secretToAuthenticateInitiator', 'chapInfo_secretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
--
-- 'secretToAuthenticateTarget', 'chapInfo_secretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g., Windows client).
--
-- 'targetARN', 'chapInfo_targetARN' - The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
newChapInfo ::
  ChapInfo
newChapInfo =
  ChapInfo'
    { initiatorName = Prelude.Nothing,
      secretToAuthenticateInitiator = Prelude.Nothing,
      secretToAuthenticateTarget = Prelude.Nothing,
      targetARN = Prelude.Nothing
    }

-- | The iSCSI initiator that connects to the target.
chapInfo_initiatorName :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_initiatorName = Lens.lens (\ChapInfo' {initiatorName} -> initiatorName) (\s@ChapInfo' {} a -> s {initiatorName = a} :: ChapInfo)

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
chapInfo_secretToAuthenticateInitiator :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_secretToAuthenticateInitiator = Lens.lens (\ChapInfo' {secretToAuthenticateInitiator} -> secretToAuthenticateInitiator) (\s@ChapInfo' {} a -> s {secretToAuthenticateInitiator = a} :: ChapInfo) Prelude.. Lens.mapping Data._Sensitive

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g., Windows client).
chapInfo_secretToAuthenticateTarget :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_secretToAuthenticateTarget = Lens.lens (\ChapInfo' {secretToAuthenticateTarget} -> secretToAuthenticateTarget) (\s@ChapInfo' {} a -> s {secretToAuthenticateTarget = a} :: ChapInfo) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the volume.
--
-- Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
chapInfo_targetARN :: Lens.Lens' ChapInfo (Prelude.Maybe Prelude.Text)
chapInfo_targetARN = Lens.lens (\ChapInfo' {targetARN} -> targetARN) (\s@ChapInfo' {} a -> s {targetARN = a} :: ChapInfo)

instance Data.FromJSON ChapInfo where
  parseJSON =
    Data.withObject
      "ChapInfo"
      ( \x ->
          ChapInfo'
            Prelude.<$> (x Data..:? "InitiatorName")
            Prelude.<*> (x Data..:? "SecretToAuthenticateInitiator")
            Prelude.<*> (x Data..:? "SecretToAuthenticateTarget")
            Prelude.<*> (x Data..:? "TargetARN")
      )

instance Prelude.Hashable ChapInfo where
  hashWithSalt _salt ChapInfo' {..} =
    _salt `Prelude.hashWithSalt` initiatorName
      `Prelude.hashWithSalt` secretToAuthenticateInitiator
      `Prelude.hashWithSalt` secretToAuthenticateTarget
      `Prelude.hashWithSalt` targetARN

instance Prelude.NFData ChapInfo where
  rnf ChapInfo' {..} =
    Prelude.rnf initiatorName
      `Prelude.seq` Prelude.rnf secretToAuthenticateInitiator
      `Prelude.seq` Prelude.rnf secretToAuthenticateTarget
      `Prelude.seq` Prelude.rnf targetARN
