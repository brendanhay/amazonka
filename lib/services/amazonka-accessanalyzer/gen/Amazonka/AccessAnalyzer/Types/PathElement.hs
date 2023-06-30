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
-- Module      : Amazonka.AccessAnalyzer.Types.PathElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.PathElement where

import Amazonka.AccessAnalyzer.Types.Substring
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single element in a path through the JSON representation of a policy.
--
-- /See:/ 'newPathElement' smart constructor.
data PathElement = PathElement'
  { -- | Refers to an index in a JSON array.
    index :: Prelude.Maybe Prelude.Int,
    -- | Refers to a key in a JSON object.
    key :: Prelude.Maybe Prelude.Text,
    -- | Refers to a substring of a literal string in a JSON object.
    substring :: Prelude.Maybe Substring,
    -- | Refers to the value associated with a given key in a JSON object.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'pathElement_index' - Refers to an index in a JSON array.
--
-- 'key', 'pathElement_key' - Refers to a key in a JSON object.
--
-- 'substring', 'pathElement_substring' - Refers to a substring of a literal string in a JSON object.
--
-- 'value', 'pathElement_value' - Refers to the value associated with a given key in a JSON object.
newPathElement ::
  PathElement
newPathElement =
  PathElement'
    { index = Prelude.Nothing,
      key = Prelude.Nothing,
      substring = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Refers to an index in a JSON array.
pathElement_index :: Lens.Lens' PathElement (Prelude.Maybe Prelude.Int)
pathElement_index = Lens.lens (\PathElement' {index} -> index) (\s@PathElement' {} a -> s {index = a} :: PathElement)

-- | Refers to a key in a JSON object.
pathElement_key :: Lens.Lens' PathElement (Prelude.Maybe Prelude.Text)
pathElement_key = Lens.lens (\PathElement' {key} -> key) (\s@PathElement' {} a -> s {key = a} :: PathElement)

-- | Refers to a substring of a literal string in a JSON object.
pathElement_substring :: Lens.Lens' PathElement (Prelude.Maybe Substring)
pathElement_substring = Lens.lens (\PathElement' {substring} -> substring) (\s@PathElement' {} a -> s {substring = a} :: PathElement)

-- | Refers to the value associated with a given key in a JSON object.
pathElement_value :: Lens.Lens' PathElement (Prelude.Maybe Prelude.Text)
pathElement_value = Lens.lens (\PathElement' {value} -> value) (\s@PathElement' {} a -> s {value = a} :: PathElement)

instance Data.FromJSON PathElement where
  parseJSON =
    Data.withObject
      "PathElement"
      ( \x ->
          PathElement'
            Prelude.<$> (x Data..:? "index")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "substring")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable PathElement where
  hashWithSalt _salt PathElement' {..} =
    _salt
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` substring
      `Prelude.hashWithSalt` value

instance Prelude.NFData PathElement where
  rnf PathElement' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf substring
      `Prelude.seq` Prelude.rnf value
