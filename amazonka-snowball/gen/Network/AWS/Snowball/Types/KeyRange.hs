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
-- Module      : Network.AWS.Snowball.Types.KeyRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.KeyRange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains a key range. For export jobs, a @S3Resource@ object can have an
-- optional @KeyRange@ value. The length of the range is defined at job
-- creation, and has either an inclusive @BeginMarker@, an inclusive
-- @EndMarker@, or both. Ranges are UTF-8 binary sorted.
--
-- /See:/ 'newKeyRange' smart constructor.
data KeyRange = KeyRange'
  { -- | The key that ends an optional key range for an export job. Ranges are
    -- inclusive and UTF-8 binary sorted.
    endMarker :: Prelude.Maybe Prelude.Text,
    -- | The key that starts an optional key range for an export job. Ranges are
    -- inclusive and UTF-8 binary sorted.
    beginMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endMarker', 'keyRange_endMarker' - The key that ends an optional key range for an export job. Ranges are
-- inclusive and UTF-8 binary sorted.
--
-- 'beginMarker', 'keyRange_beginMarker' - The key that starts an optional key range for an export job. Ranges are
-- inclusive and UTF-8 binary sorted.
newKeyRange ::
  KeyRange
newKeyRange =
  KeyRange'
    { endMarker = Prelude.Nothing,
      beginMarker = Prelude.Nothing
    }

-- | The key that ends an optional key range for an export job. Ranges are
-- inclusive and UTF-8 binary sorted.
keyRange_endMarker :: Lens.Lens' KeyRange (Prelude.Maybe Prelude.Text)
keyRange_endMarker = Lens.lens (\KeyRange' {endMarker} -> endMarker) (\s@KeyRange' {} a -> s {endMarker = a} :: KeyRange)

-- | The key that starts an optional key range for an export job. Ranges are
-- inclusive and UTF-8 binary sorted.
keyRange_beginMarker :: Lens.Lens' KeyRange (Prelude.Maybe Prelude.Text)
keyRange_beginMarker = Lens.lens (\KeyRange' {beginMarker} -> beginMarker) (\s@KeyRange' {} a -> s {beginMarker = a} :: KeyRange)

instance Prelude.FromJSON KeyRange where
  parseJSON =
    Prelude.withObject
      "KeyRange"
      ( \x ->
          KeyRange'
            Prelude.<$> (x Prelude..:? "EndMarker")
            Prelude.<*> (x Prelude..:? "BeginMarker")
      )

instance Prelude.Hashable KeyRange

instance Prelude.NFData KeyRange

instance Prelude.ToJSON KeyRange where
  toJSON KeyRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EndMarker" Prelude..=) Prelude.<$> endMarker,
            ("BeginMarker" Prelude..=) Prelude.<$> beginMarker
          ]
      )
