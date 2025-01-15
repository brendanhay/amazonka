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
-- Module      : Amazonka.IVS.Types.PlaybackKeyPairSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.PlaybackKeyPairSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a playback key pair.
--
-- /See:/ 'newPlaybackKeyPairSummary' smart constructor.
data PlaybackKeyPairSummary = PlaybackKeyPairSummary'
  { -- | Key-pair ARN.
    arn :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'PlaybackKeyPairSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'playbackKeyPairSummary_arn' - Key-pair ARN.
--
-- 'name', 'playbackKeyPairSummary_name' - Playback-key-pair name. The value does not need to be unique.
--
-- 'tags', 'playbackKeyPairSummary_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
newPlaybackKeyPairSummary ::
  PlaybackKeyPairSummary
newPlaybackKeyPairSummary =
  PlaybackKeyPairSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Key-pair ARN.
playbackKeyPairSummary_arn :: Lens.Lens' PlaybackKeyPairSummary (Prelude.Maybe Prelude.Text)
playbackKeyPairSummary_arn = Lens.lens (\PlaybackKeyPairSummary' {arn} -> arn) (\s@PlaybackKeyPairSummary' {} a -> s {arn = a} :: PlaybackKeyPairSummary)

-- | Playback-key-pair name. The value does not need to be unique.
playbackKeyPairSummary_name :: Lens.Lens' PlaybackKeyPairSummary (Prelude.Maybe Prelude.Text)
playbackKeyPairSummary_name = Lens.lens (\PlaybackKeyPairSummary' {name} -> name) (\s@PlaybackKeyPairSummary' {} a -> s {name = a} :: PlaybackKeyPairSummary)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
playbackKeyPairSummary_tags :: Lens.Lens' PlaybackKeyPairSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
playbackKeyPairSummary_tags = Lens.lens (\PlaybackKeyPairSummary' {tags} -> tags) (\s@PlaybackKeyPairSummary' {} a -> s {tags = a} :: PlaybackKeyPairSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PlaybackKeyPairSummary where
  parseJSON =
    Data.withObject
      "PlaybackKeyPairSummary"
      ( \x ->
          PlaybackKeyPairSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PlaybackKeyPairSummary where
  hashWithSalt _salt PlaybackKeyPairSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData PlaybackKeyPairSummary where
  rnf PlaybackKeyPairSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf tags
