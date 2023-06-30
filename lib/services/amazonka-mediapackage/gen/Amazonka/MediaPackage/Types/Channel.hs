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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.Channel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.EgressAccessLogs
import Amazonka.MediaPackage.Types.HlsIngest
import Amazonka.MediaPackage.Types.IngressAccessLogs
import qualified Amazonka.Prelude as Prelude

-- | A Channel resource configuration.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
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
-- 'arn', 'channel_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'description', 'channel_description' - A short text description of the Channel.
--
-- 'egressAccessLogs', 'channel_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'channel_hlsIngest' - Undocumented member.
--
-- 'id', 'channel_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'channel_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'channel_tags' - Undocumented member.
newChannel ::
  Channel
newChannel =
  Channel'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      id = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) assigned to the Channel.
channel_arn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | A short text description of the Channel.
channel_description :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_description = Lens.lens (\Channel' {description} -> description) (\s@Channel' {} a -> s {description = a} :: Channel)

-- | Undocumented member.
channel_egressAccessLogs :: Lens.Lens' Channel (Prelude.Maybe EgressAccessLogs)
channel_egressAccessLogs = Lens.lens (\Channel' {egressAccessLogs} -> egressAccessLogs) (\s@Channel' {} a -> s {egressAccessLogs = a} :: Channel)

-- | Undocumented member.
channel_hlsIngest :: Lens.Lens' Channel (Prelude.Maybe HlsIngest)
channel_hlsIngest = Lens.lens (\Channel' {hlsIngest} -> hlsIngest) (\s@Channel' {} a -> s {hlsIngest = a} :: Channel)

-- | The ID of the Channel.
channel_id :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_id = Lens.lens (\Channel' {id} -> id) (\s@Channel' {} a -> s {id = a} :: Channel)

-- | Undocumented member.
channel_ingressAccessLogs :: Lens.Lens' Channel (Prelude.Maybe IngressAccessLogs)
channel_ingressAccessLogs = Lens.lens (\Channel' {ingressAccessLogs} -> ingressAccessLogs) (\s@Channel' {} a -> s {ingressAccessLogs = a} :: Channel)

-- | Undocumented member.
channel_tags :: Lens.Lens' Channel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Channel where
  parseJSON =
    Data.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "egressAccessLogs")
            Prelude.<*> (x Data..:? "hlsIngest")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "ingressAccessLogs")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` egressAccessLogs
      `Prelude.hashWithSalt` hlsIngest
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` ingressAccessLogs
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf hlsIngest
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ingressAccessLogs
      `Prelude.seq` Prelude.rnf tags
