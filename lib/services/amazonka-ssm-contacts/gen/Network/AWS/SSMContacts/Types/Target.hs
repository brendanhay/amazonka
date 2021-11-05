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
-- Module      : Network.AWS.SSMContacts.Types.Target
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMContacts.Types.Target where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMContacts.Types.ChannelTargetInfo
import Network.AWS.SSMContacts.Types.ContactTargetInfo

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

instance Core.FromJSON Target where
  parseJSON =
    Core.withObject
      "Target"
      ( \x ->
          Target'
            Prelude.<$> (x Core..:? "ChannelTargetInfo")
            Prelude.<*> (x Core..:? "ContactTargetInfo")
      )

instance Prelude.Hashable Target

instance Prelude.NFData Target

instance Core.ToJSON Target where
  toJSON Target' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ChannelTargetInfo" Core..=)
              Prelude.<$> channelTargetInfo,
            ("ContactTargetInfo" Core..=)
              Prelude.<$> contactTargetInfo
          ]
      )
