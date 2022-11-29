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
-- Module      : Amazonka.SSMContacts.Types.ChannelTargetInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.ChannelTargetInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the contact channel that Incident Manager uses to
-- engage the contact.
--
-- /See:/ 'newChannelTargetInfo' smart constructor.
data ChannelTargetInfo = ChannelTargetInfo'
  { -- | The number of minutes to wait to retry sending engagement in the case
    -- the engagement initially fails.
    retryIntervalInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the contact channel.
    contactChannelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelTargetInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retryIntervalInMinutes', 'channelTargetInfo_retryIntervalInMinutes' - The number of minutes to wait to retry sending engagement in the case
-- the engagement initially fails.
--
-- 'contactChannelId', 'channelTargetInfo_contactChannelId' - The Amazon Resource Name (ARN) of the contact channel.
newChannelTargetInfo ::
  -- | 'contactChannelId'
  Prelude.Text ->
  ChannelTargetInfo
newChannelTargetInfo pContactChannelId_ =
  ChannelTargetInfo'
    { retryIntervalInMinutes =
        Prelude.Nothing,
      contactChannelId = pContactChannelId_
    }

-- | The number of minutes to wait to retry sending engagement in the case
-- the engagement initially fails.
channelTargetInfo_retryIntervalInMinutes :: Lens.Lens' ChannelTargetInfo (Prelude.Maybe Prelude.Natural)
channelTargetInfo_retryIntervalInMinutes = Lens.lens (\ChannelTargetInfo' {retryIntervalInMinutes} -> retryIntervalInMinutes) (\s@ChannelTargetInfo' {} a -> s {retryIntervalInMinutes = a} :: ChannelTargetInfo)

-- | The Amazon Resource Name (ARN) of the contact channel.
channelTargetInfo_contactChannelId :: Lens.Lens' ChannelTargetInfo Prelude.Text
channelTargetInfo_contactChannelId = Lens.lens (\ChannelTargetInfo' {contactChannelId} -> contactChannelId) (\s@ChannelTargetInfo' {} a -> s {contactChannelId = a} :: ChannelTargetInfo)

instance Core.FromJSON ChannelTargetInfo where
  parseJSON =
    Core.withObject
      "ChannelTargetInfo"
      ( \x ->
          ChannelTargetInfo'
            Prelude.<$> (x Core..:? "RetryIntervalInMinutes")
            Prelude.<*> (x Core..: "ContactChannelId")
      )

instance Prelude.Hashable ChannelTargetInfo where
  hashWithSalt _salt ChannelTargetInfo' {..} =
    _salt `Prelude.hashWithSalt` retryIntervalInMinutes
      `Prelude.hashWithSalt` contactChannelId

instance Prelude.NFData ChannelTargetInfo where
  rnf ChannelTargetInfo' {..} =
    Prelude.rnf retryIntervalInMinutes
      `Prelude.seq` Prelude.rnf contactChannelId

instance Core.ToJSON ChannelTargetInfo where
  toJSON ChannelTargetInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetryIntervalInMinutes" Core..=)
              Prelude.<$> retryIntervalInMinutes,
            Prelude.Just
              ("ContactChannelId" Core..= contactChannelId)
          ]
      )
