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
-- Module      : Amazonka.IoTAnalytics.Types.ChannelMessages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ChannelMessages where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more sets of channel messages.
--
-- /See:/ 'newChannelMessages' smart constructor.
data ChannelMessages = ChannelMessages'
  { -- | Specifies one or more keys that identify the Amazon Simple Storage
    -- Service (Amazon S3) objects that save your channel messages.
    --
    -- You must use the full path for the key.
    --
    -- Example path:
    -- @channel\/mychannel\/__dt=2020-02-29 00:00:00\/1582940490000_1582940520000_123456789012_mychannel_0_2118.0.json.gz@
    s3Paths :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Paths', 'channelMessages_s3Paths' - Specifies one or more keys that identify the Amazon Simple Storage
-- Service (Amazon S3) objects that save your channel messages.
--
-- You must use the full path for the key.
--
-- Example path:
-- @channel\/mychannel\/__dt=2020-02-29 00:00:00\/1582940490000_1582940520000_123456789012_mychannel_0_2118.0.json.gz@
newChannelMessages ::
  ChannelMessages
newChannelMessages =
  ChannelMessages' {s3Paths = Prelude.Nothing}

-- | Specifies one or more keys that identify the Amazon Simple Storage
-- Service (Amazon S3) objects that save your channel messages.
--
-- You must use the full path for the key.
--
-- Example path:
-- @channel\/mychannel\/__dt=2020-02-29 00:00:00\/1582940490000_1582940520000_123456789012_mychannel_0_2118.0.json.gz@
channelMessages_s3Paths :: Lens.Lens' ChannelMessages (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
channelMessages_s3Paths = Lens.lens (\ChannelMessages' {s3Paths} -> s3Paths) (\s@ChannelMessages' {} a -> s {s3Paths = a} :: ChannelMessages) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ChannelMessages where
  hashWithSalt _salt ChannelMessages' {..} =
    _salt `Prelude.hashWithSalt` s3Paths

instance Prelude.NFData ChannelMessages where
  rnf ChannelMessages' {..} = Prelude.rnf s3Paths

instance Data.ToJSON ChannelMessages where
  toJSON ChannelMessages' {..} =
    Data.object
      ( Prelude.catMaybes
          [("s3Paths" Data..=) Prelude.<$> s3Paths]
      )
