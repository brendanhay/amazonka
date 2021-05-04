{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DynamoDB.Types.StreamSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.StreamSpecification where

import Network.AWS.DynamoDB.Types.StreamViewType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the DynamoDB Streams configuration for a table in DynamoDB.
--
-- /See:/ 'newStreamSpecification' smart constructor.
data StreamSpecification = StreamSpecification'
  { -- | When an item in the table is modified, @StreamViewType@ determines what
    -- information is written to the stream for this table. Valid values for
    -- @StreamViewType@ are:
    --
    -- -   @KEYS_ONLY@ - Only the key attributes of the modified item are
    --     written to the stream.
    --
    -- -   @NEW_IMAGE@ - The entire item, as it appears after it was modified,
    --     is written to the stream.
    --
    -- -   @OLD_IMAGE@ - The entire item, as it appeared before it was
    --     modified, is written to the stream.
    --
    -- -   @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the
    --     item are written to the stream.
    streamViewType :: Prelude.Maybe StreamViewType,
    -- | Indicates whether DynamoDB Streams is enabled (true) or disabled (false)
    -- on the table.
    streamEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StreamSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamViewType', 'streamSpecification_streamViewType' - When an item in the table is modified, @StreamViewType@ determines what
-- information is written to the stream for this table. Valid values for
-- @StreamViewType@ are:
--
-- -   @KEYS_ONLY@ - Only the key attributes of the modified item are
--     written to the stream.
--
-- -   @NEW_IMAGE@ - The entire item, as it appears after it was modified,
--     is written to the stream.
--
-- -   @OLD_IMAGE@ - The entire item, as it appeared before it was
--     modified, is written to the stream.
--
-- -   @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the
--     item are written to the stream.
--
-- 'streamEnabled', 'streamSpecification_streamEnabled' - Indicates whether DynamoDB Streams is enabled (true) or disabled (false)
-- on the table.
newStreamSpecification ::
  -- | 'streamEnabled'
  Prelude.Bool ->
  StreamSpecification
newStreamSpecification pStreamEnabled_ =
  StreamSpecification'
    { streamViewType =
        Prelude.Nothing,
      streamEnabled = pStreamEnabled_
    }

-- | When an item in the table is modified, @StreamViewType@ determines what
-- information is written to the stream for this table. Valid values for
-- @StreamViewType@ are:
--
-- -   @KEYS_ONLY@ - Only the key attributes of the modified item are
--     written to the stream.
--
-- -   @NEW_IMAGE@ - The entire item, as it appears after it was modified,
--     is written to the stream.
--
-- -   @OLD_IMAGE@ - The entire item, as it appeared before it was
--     modified, is written to the stream.
--
-- -   @NEW_AND_OLD_IMAGES@ - Both the new and the old item images of the
--     item are written to the stream.
streamSpecification_streamViewType :: Lens.Lens' StreamSpecification (Prelude.Maybe StreamViewType)
streamSpecification_streamViewType = Lens.lens (\StreamSpecification' {streamViewType} -> streamViewType) (\s@StreamSpecification' {} a -> s {streamViewType = a} :: StreamSpecification)

-- | Indicates whether DynamoDB Streams is enabled (true) or disabled (false)
-- on the table.
streamSpecification_streamEnabled :: Lens.Lens' StreamSpecification Prelude.Bool
streamSpecification_streamEnabled = Lens.lens (\StreamSpecification' {streamEnabled} -> streamEnabled) (\s@StreamSpecification' {} a -> s {streamEnabled = a} :: StreamSpecification)

instance Prelude.FromJSON StreamSpecification where
  parseJSON =
    Prelude.withObject
      "StreamSpecification"
      ( \x ->
          StreamSpecification'
            Prelude.<$> (x Prelude..:? "StreamViewType")
            Prelude.<*> (x Prelude..: "StreamEnabled")
      )

instance Prelude.Hashable StreamSpecification

instance Prelude.NFData StreamSpecification

instance Prelude.ToJSON StreamSpecification where
  toJSON StreamSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StreamViewType" Prelude..=)
              Prelude.<$> streamViewType,
            Prelude.Just
              ("StreamEnabled" Prelude..= streamEnabled)
          ]
      )
