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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelMessages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelMessages where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies one or more sets of channel messages.
--
-- /See:/ 'newChannelMessages' smart constructor.
data ChannelMessages = ChannelMessages'
  { -- | Specifies one or more keys that identify the Amazon Simple Storage
    -- Service (Amazon S3) objects that save your channel messages.
    s3Paths :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newChannelMessages ::
  ChannelMessages
newChannelMessages =
  ChannelMessages' {s3Paths = Core.Nothing}

-- | Specifies one or more keys that identify the Amazon Simple Storage
-- Service (Amazon S3) objects that save your channel messages.
channelMessages_s3Paths :: Lens.Lens' ChannelMessages (Core.Maybe (Core.NonEmpty Core.Text))
channelMessages_s3Paths = Lens.lens (\ChannelMessages' {s3Paths} -> s3Paths) (\s@ChannelMessages' {} a -> s {s3Paths = a} :: ChannelMessages) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable ChannelMessages

instance Core.NFData ChannelMessages

instance Core.ToJSON ChannelMessages where
  toJSON ChannelMessages' {..} =
    Core.object
      ( Core.catMaybes
          [("s3Paths" Core..=) Core.<$> s3Paths]
      )
