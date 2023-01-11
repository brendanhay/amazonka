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
-- Module      : Amazonka.IoTAnalytics.Types.ChannelActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ChannelActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The activity that determines the source of the messages to be processed.
--
-- /See:/ 'newChannelActivity' smart constructor.
data ChannelActivity = ChannelActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel activity.
    name :: Prelude.Text,
    -- | The name of the channel from which the messages are processed.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'channelActivity_next' - The next activity in the pipeline.
--
-- 'name', 'channelActivity_name' - The name of the channel activity.
--
-- 'channelName', 'channelActivity_channelName' - The name of the channel from which the messages are processed.
newChannelActivity ::
  -- | 'name'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  ChannelActivity
newChannelActivity pName_ pChannelName_ =
  ChannelActivity'
    { next = Prelude.Nothing,
      name = pName_,
      channelName = pChannelName_
    }

-- | The next activity in the pipeline.
channelActivity_next :: Lens.Lens' ChannelActivity (Prelude.Maybe Prelude.Text)
channelActivity_next = Lens.lens (\ChannelActivity' {next} -> next) (\s@ChannelActivity' {} a -> s {next = a} :: ChannelActivity)

-- | The name of the channel activity.
channelActivity_name :: Lens.Lens' ChannelActivity Prelude.Text
channelActivity_name = Lens.lens (\ChannelActivity' {name} -> name) (\s@ChannelActivity' {} a -> s {name = a} :: ChannelActivity)

-- | The name of the channel from which the messages are processed.
channelActivity_channelName :: Lens.Lens' ChannelActivity Prelude.Text
channelActivity_channelName = Lens.lens (\ChannelActivity' {channelName} -> channelName) (\s@ChannelActivity' {} a -> s {channelName = a} :: ChannelActivity)

instance Data.FromJSON ChannelActivity where
  parseJSON =
    Data.withObject
      "ChannelActivity"
      ( \x ->
          ChannelActivity'
            Prelude.<$> (x Data..:? "next")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "channelName")
      )

instance Prelude.Hashable ChannelActivity where
  hashWithSalt _salt ChannelActivity' {..} =
    _salt `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData ChannelActivity where
  rnf ChannelActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToJSON ChannelActivity where
  toJSON ChannelActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("next" Data..=) Prelude.<$> next,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("channelName" Data..= channelName)
          ]
      )
