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
-- Module      : Amazonka.MediaPackage.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.Channel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackage.Types.EgressAccessLogs
import Amazonka.MediaPackage.Types.HlsIngest
import Amazonka.MediaPackage.Types.IngressAccessLogs
import qualified Amazonka.Prelude as Prelude

-- | A Channel resource configuration.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingressAccessLogs', 'channel_ingressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'channel_hlsIngest' - Undocumented member.
--
-- 'arn', 'channel_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'channel_id' - The ID of the Channel.
--
-- 'description', 'channel_description' - A short text description of the Channel.
--
-- 'egressAccessLogs', 'channel_egressAccessLogs' - Undocumented member.
--
-- 'tags', 'channel_tags' - Undocumented member.
newChannel ::
  Channel
newChannel =
  Channel'
    { ingressAccessLogs = Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
channel_ingressAccessLogs :: Lens.Lens' Channel (Prelude.Maybe IngressAccessLogs)
channel_ingressAccessLogs = Lens.lens (\Channel' {ingressAccessLogs} -> ingressAccessLogs) (\s@Channel' {} a -> s {ingressAccessLogs = a} :: Channel)

-- | Undocumented member.
channel_hlsIngest :: Lens.Lens' Channel (Prelude.Maybe HlsIngest)
channel_hlsIngest = Lens.lens (\Channel' {hlsIngest} -> hlsIngest) (\s@Channel' {} a -> s {hlsIngest = a} :: Channel)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
channel_arn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The ID of the Channel.
channel_id :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_id = Lens.lens (\Channel' {id} -> id) (\s@Channel' {} a -> s {id = a} :: Channel)

-- | A short text description of the Channel.
channel_description :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_description = Lens.lens (\Channel' {description} -> description) (\s@Channel' {} a -> s {description = a} :: Channel)

-- | Undocumented member.
channel_egressAccessLogs :: Lens.Lens' Channel (Prelude.Maybe EgressAccessLogs)
channel_egressAccessLogs = Lens.lens (\Channel' {egressAccessLogs} -> egressAccessLogs) (\s@Channel' {} a -> s {egressAccessLogs = a} :: Channel)

-- | Undocumented member.
channel_tags :: Lens.Lens' Channel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Core..:? "ingressAccessLogs")
            Prelude.<*> (x Core..:? "hlsIngest")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "egressAccessLogs")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Channel

instance Prelude.NFData Channel
