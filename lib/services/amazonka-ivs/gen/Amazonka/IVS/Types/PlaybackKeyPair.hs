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
-- Module      : Amazonka.IVS.Types.PlaybackKeyPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.PlaybackKeyPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key pair used to sign and validate a playback authorization token.
--
-- /See:/ 'newPlaybackKeyPair' smart constructor.
data PlaybackKeyPair = PlaybackKeyPair'
  { -- | Key-pair ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Key-pair identifier.
    fingerprint :: Prelude.Maybe Prelude.Text,
    -- | Playback-key-pair name. The value does not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlaybackKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'playbackKeyPair_arn' - Key-pair ARN.
--
-- 'fingerprint', 'playbackKeyPair_fingerprint' - Key-pair identifier.
--
-- 'name', 'playbackKeyPair_name' - Playback-key-pair name. The value does not need to be unique.
--
-- 'tags', 'playbackKeyPair_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
newPlaybackKeyPair ::
  PlaybackKeyPair
newPlaybackKeyPair =
  PlaybackKeyPair'
    { arn = Prelude.Nothing,
      fingerprint = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Key-pair ARN.
playbackKeyPair_arn :: Lens.Lens' PlaybackKeyPair (Prelude.Maybe Prelude.Text)
playbackKeyPair_arn = Lens.lens (\PlaybackKeyPair' {arn} -> arn) (\s@PlaybackKeyPair' {} a -> s {arn = a} :: PlaybackKeyPair)

-- | Key-pair identifier.
playbackKeyPair_fingerprint :: Lens.Lens' PlaybackKeyPair (Prelude.Maybe Prelude.Text)
playbackKeyPair_fingerprint = Lens.lens (\PlaybackKeyPair' {fingerprint} -> fingerprint) (\s@PlaybackKeyPair' {} a -> s {fingerprint = a} :: PlaybackKeyPair)

-- | Playback-key-pair name. The value does not need to be unique.
playbackKeyPair_name :: Lens.Lens' PlaybackKeyPair (Prelude.Maybe Prelude.Text)
playbackKeyPair_name = Lens.lens (\PlaybackKeyPair' {name} -> name) (\s@PlaybackKeyPair' {} a -> s {name = a} :: PlaybackKeyPair)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
playbackKeyPair_tags :: Lens.Lens' PlaybackKeyPair (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
playbackKeyPair_tags = Lens.lens (\PlaybackKeyPair' {tags} -> tags) (\s@PlaybackKeyPair' {} a -> s {tags = a} :: PlaybackKeyPair) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PlaybackKeyPair where
  parseJSON =
    Data.withObject
      "PlaybackKeyPair"
      ( \x ->
          PlaybackKeyPair'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "fingerprint")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PlaybackKeyPair where
  hashWithSalt _salt PlaybackKeyPair' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` fingerprint
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData PlaybackKeyPair where
  rnf PlaybackKeyPair' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf fingerprint
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
