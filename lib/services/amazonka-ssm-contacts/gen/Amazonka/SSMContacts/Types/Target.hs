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
-- Module      : Amazonka.SSMContacts.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ChannelTargetInfo
import Amazonka.SSMContacts.Types.ContactTargetInfo

-- | The contact or contact channel that\'s being engaged.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | Information about the contact channel Incident Manager is engaging.
    channelTargetInfo :: Prelude.Maybe ChannelTargetInfo,
    -- | Information about the contact that Incident Manager is engaging.
    contactTargetInfo :: Prelude.Maybe ContactTargetInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelTargetInfo', 'target_channelTargetInfo' - Information about the contact channel Incident Manager is engaging.
--
-- 'contactTargetInfo', 'target_contactTargetInfo' - Information about the contact that Incident Manager is engaging.
newTarget ::
  Target
newTarget =
  Target'
    { channelTargetInfo = Prelude.Nothing,
      contactTargetInfo = Prelude.Nothing
    }

-- | Information about the contact channel Incident Manager is engaging.
target_channelTargetInfo :: Lens.Lens' Target (Prelude.Maybe ChannelTargetInfo)
target_channelTargetInfo = Lens.lens (\Target' {channelTargetInfo} -> channelTargetInfo) (\s@Target' {} a -> s {channelTargetInfo = a} :: Target)

-- | Information about the contact that Incident Manager is engaging.
target_contactTargetInfo :: Lens.Lens' Target (Prelude.Maybe ContactTargetInfo)
target_contactTargetInfo = Lens.lens (\Target' {contactTargetInfo} -> contactTargetInfo) (\s@Target' {} a -> s {contactTargetInfo = a} :: Target)

instance Data.FromJSON Target where
  parseJSON =
    Data.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Data..:? "ChannelTargetInfo")
            Prelude.<*> (x Data..:? "ContactTargetInfo")
      )

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt
      `Prelude.hashWithSalt` channelTargetInfo
      `Prelude.hashWithSalt` contactTargetInfo

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf channelTargetInfo
      `Prelude.seq` Prelude.rnf contactTargetInfo

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelTargetInfo" Data..=)
              Prelude.<$> channelTargetInfo,
            ("ContactTargetInfo" Data..=)
              Prelude.<$> contactTargetInfo
          ]
      )
