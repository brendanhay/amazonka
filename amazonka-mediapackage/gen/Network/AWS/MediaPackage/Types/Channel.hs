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
-- Module      : Network.AWS.MediaPackage.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Channel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.EgressAccessLogs
import Network.AWS.MediaPackage.Types.HlsIngest
import Network.AWS.MediaPackage.Types.IngressAccessLogs

-- | A Channel resource configuration.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { egressAccessLogs :: Core.Maybe EgressAccessLogs,
    hlsIngest :: Core.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the Channel.
    id :: Core.Maybe Core.Text,
    ingressAccessLogs :: Core.Maybe IngressAccessLogs,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'channel_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'channel_hlsIngest' - Undocumented member.
--
-- 'arn', 'channel_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'channel_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'channel_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'channel_tags' - Undocumented member.
--
-- 'description', 'channel_description' - A short text description of the Channel.
newChannel ::
  Channel
newChannel =
  Channel'
    { egressAccessLogs = Core.Nothing,
      hlsIngest = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      ingressAccessLogs = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing
    }

-- | Undocumented member.
channel_egressAccessLogs :: Lens.Lens' Channel (Core.Maybe EgressAccessLogs)
channel_egressAccessLogs = Lens.lens (\Channel' {egressAccessLogs} -> egressAccessLogs) (\s@Channel' {} a -> s {egressAccessLogs = a} :: Channel)

-- | Undocumented member.
channel_hlsIngest :: Lens.Lens' Channel (Core.Maybe HlsIngest)
channel_hlsIngest = Lens.lens (\Channel' {hlsIngest} -> hlsIngest) (\s@Channel' {} a -> s {hlsIngest = a} :: Channel)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
channel_arn :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The ID of the Channel.
channel_id :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_id = Lens.lens (\Channel' {id} -> id) (\s@Channel' {} a -> s {id = a} :: Channel)

-- | Undocumented member.
channel_ingressAccessLogs :: Lens.Lens' Channel (Core.Maybe IngressAccessLogs)
channel_ingressAccessLogs = Lens.lens (\Channel' {ingressAccessLogs} -> ingressAccessLogs) (\s@Channel' {} a -> s {ingressAccessLogs = a} :: Channel)

-- | Undocumented member.
channel_tags :: Lens.Lens' Channel (Core.Maybe (Core.HashMap Core.Text Core.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
channel_description :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_description = Lens.lens (\Channel' {description} -> description) (\s@Channel' {} a -> s {description = a} :: Channel)

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Core.<$> (x Core..:? "egressAccessLogs")
            Core.<*> (x Core..:? "hlsIngest")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "ingressAccessLogs")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable Channel

instance Core.NFData Channel
