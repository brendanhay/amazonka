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
-- Module      : Amazonka.DynamoDB.Types.StreamSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.StreamSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.StreamViewType
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON StreamSpecification where
  parseJSON =
    Data.withObject
      "StreamSpecification"
      ( \x ->
          StreamSpecification'
            Prelude.<$> (x Data..:? "StreamViewType")
            Prelude.<*> (x Data..: "StreamEnabled")
      )

instance Prelude.Hashable StreamSpecification where
  hashWithSalt _salt StreamSpecification' {..} =
    _salt `Prelude.hashWithSalt` streamViewType
      `Prelude.hashWithSalt` streamEnabled

instance Prelude.NFData StreamSpecification where
  rnf StreamSpecification' {..} =
    Prelude.rnf streamViewType
      `Prelude.seq` Prelude.rnf streamEnabled

instance Data.ToJSON StreamSpecification where
  toJSON StreamSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamViewType" Data..=)
              Prelude.<$> streamViewType,
            Prelude.Just
              ("StreamEnabled" Data..= streamEnabled)
          ]
      )
