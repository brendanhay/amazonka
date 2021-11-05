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
-- Module      : Network.AWS.IVS.Types.StreamKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IVS.Types.StreamKey where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Object specifying a stream key.
--
-- /See:/ 'newStreamKey' smart constructor.
data StreamKey = StreamKey'
  { -- | Stream-key ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Stream-key value.
    value :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Channel ARN for the stream.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | Array of 1-50 maps, each of the form @string:string (key:value)@.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'value', 'streamKey_value' - Stream-key value.
--
-- 'channelArn', 'streamKey_channelArn' - Channel ARN for the stream.
--
-- 'tags', 'streamKey_tags' - Array of 1-50 maps, each of the form @string:string (key:value)@.
newStreamKey ::
  StreamKey
newStreamKey =
  StreamKey'
    { arn = Prelude.Nothing,
      value = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Stream-key ARN.
streamKey_arn :: Lens.Lens' StreamKey (Prelude.Maybe Prelude.Text)
streamKey_arn = Lens.lens (\StreamKey' {arn} -> arn) (\s@StreamKey' {} a -> s {arn = a} :: StreamKey)

-- | Stream-key value.
streamKey_value :: Lens.Lens' StreamKey (Prelude.Maybe Prelude.Text)
streamKey_value = Lens.lens (\StreamKey' {value} -> value) (\s@StreamKey' {} a -> s {value = a} :: StreamKey) Prelude.. Lens.mapping Core._Sensitive

-- | Channel ARN for the stream.
streamKey_channelArn :: Lens.Lens' StreamKey (Prelude.Maybe Prelude.Text)
streamKey_channelArn = Lens.lens (\StreamKey' {channelArn} -> channelArn) (\s@StreamKey' {} a -> s {channelArn = a} :: StreamKey)

-- | Array of 1-50 maps, each of the form @string:string (key:value)@.
streamKey_tags :: Lens.Lens' StreamKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamKey_tags = Lens.lens (\StreamKey' {tags} -> tags) (\s@StreamKey' {} a -> s {tags = a} :: StreamKey) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON StreamKey where
  parseJSON =
    Core.withObject
      "StreamKey"
      ( \x ->
          StreamKey'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "value")
            Prelude.<*> (x Core..:? "channelArn")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable StreamKey

instance Prelude.NFData StreamKey
