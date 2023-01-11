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
-- Module      : Amazonka.IVS.Types.StreamKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.StreamKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object specifying a stream key.
--
-- /See:/ 'newStreamKey' smart constructor.
data StreamKey = StreamKey'
  { -- | Stream-key ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Channel ARN for the stream.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- for more information, including restrictions that apply to tags and
    -- \"Tag naming limits and requirements\"; Amazon IVS has no
    -- service-specific constraints beyond what is documented there.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Stream-key value.
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'streamKey_arn' - Stream-key ARN.
--
-- 'channelArn', 'streamKey_channelArn' - Channel ARN for the stream.
--
-- 'tags', 'streamKey_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- 'value', 'streamKey_value' - Stream-key value.
newStreamKey ::
  StreamKey
newStreamKey =
  StreamKey'
    { arn = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Stream-key ARN.
streamKey_arn :: Lens.Lens' StreamKey (Prelude.Maybe Prelude.Text)
streamKey_arn = Lens.lens (\StreamKey' {arn} -> arn) (\s@StreamKey' {} a -> s {arn = a} :: StreamKey)

-- | Channel ARN for the stream.
streamKey_channelArn :: Lens.Lens' StreamKey (Prelude.Maybe Prelude.Text)
streamKey_channelArn = Lens.lens (\StreamKey' {channelArn} -> channelArn) (\s@StreamKey' {} a -> s {channelArn = a} :: StreamKey)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
streamKey_tags :: Lens.Lens' StreamKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamKey_tags = Lens.lens (\StreamKey' {tags} -> tags) (\s@StreamKey' {} a -> s {tags = a} :: StreamKey) Prelude.. Lens.mapping Lens.coerced

-- | Stream-key value.
streamKey_value :: Lens.Lens' StreamKey (Prelude.Maybe Prelude.Text)
streamKey_value = Lens.lens (\StreamKey' {value} -> value) (\s@StreamKey' {} a -> s {value = a} :: StreamKey) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON StreamKey where
  parseJSON =
    Data.withObject
      "StreamKey"
      ( \x ->
          StreamKey'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "channelArn")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable StreamKey where
  hashWithSalt _salt StreamKey' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` value

instance Prelude.NFData StreamKey where
  rnf StreamKey' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf value
