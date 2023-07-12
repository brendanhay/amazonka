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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatusStructure where

import Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores information about a message status.
--
-- /See:/ 'newChannelMessageStatusStructure' smart constructor.
data ChannelMessageStatusStructure = ChannelMessageStatusStructure'
  { -- | Contains more details about the messasge status.
    detail :: Prelude.Maybe Prelude.Text,
    -- | The message status value.
    value :: Prelude.Maybe ChannelMessageStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMessageStatusStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detail', 'channelMessageStatusStructure_detail' - Contains more details about the messasge status.
--
-- 'value', 'channelMessageStatusStructure_value' - The message status value.
newChannelMessageStatusStructure ::
  ChannelMessageStatusStructure
newChannelMessageStatusStructure =
  ChannelMessageStatusStructure'
    { detail =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Contains more details about the messasge status.
channelMessageStatusStructure_detail :: Lens.Lens' ChannelMessageStatusStructure (Prelude.Maybe Prelude.Text)
channelMessageStatusStructure_detail = Lens.lens (\ChannelMessageStatusStructure' {detail} -> detail) (\s@ChannelMessageStatusStructure' {} a -> s {detail = a} :: ChannelMessageStatusStructure)

-- | The message status value.
channelMessageStatusStructure_value :: Lens.Lens' ChannelMessageStatusStructure (Prelude.Maybe ChannelMessageStatus)
channelMessageStatusStructure_value = Lens.lens (\ChannelMessageStatusStructure' {value} -> value) (\s@ChannelMessageStatusStructure' {} a -> s {value = a} :: ChannelMessageStatusStructure)

instance Data.FromJSON ChannelMessageStatusStructure where
  parseJSON =
    Data.withObject
      "ChannelMessageStatusStructure"
      ( \x ->
          ChannelMessageStatusStructure'
            Prelude.<$> (x Data..:? "Detail")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    ChannelMessageStatusStructure
  where
  hashWithSalt _salt ChannelMessageStatusStructure' {..} =
    _salt
      `Prelude.hashWithSalt` detail
      `Prelude.hashWithSalt` value

instance Prelude.NFData ChannelMessageStatusStructure where
  rnf ChannelMessageStatusStructure' {..} =
    Prelude.rnf detail `Prelude.seq` Prelude.rnf value
