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
-- Module      : Amazonka.M2.Types.PoAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.PoAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The supported properties for a PO type data set.
--
-- /See:/ 'newPoAttributes' smart constructor.
data PoAttributes = PoAttributes'
  { -- | The character set encoding of the data set.
    encoding :: Prelude.Maybe Prelude.Text,
    -- | The format of the data set records.
    format :: Prelude.Text,
    -- | An array containing one or more filename extensions, allowing you to
    -- specify which files to be included as PDS member.
    memberFileExtensions :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PoAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoding', 'poAttributes_encoding' - The character set encoding of the data set.
--
-- 'format', 'poAttributes_format' - The format of the data set records.
--
-- 'memberFileExtensions', 'poAttributes_memberFileExtensions' - An array containing one or more filename extensions, allowing you to
-- specify which files to be included as PDS member.
newPoAttributes ::
  -- | 'format'
  Prelude.Text ->
  -- | 'memberFileExtensions'
  Prelude.NonEmpty Prelude.Text ->
  PoAttributes
newPoAttributes pFormat_ pMemberFileExtensions_ =
  PoAttributes'
    { encoding = Prelude.Nothing,
      format = pFormat_,
      memberFileExtensions =
        Lens.coerced Lens.# pMemberFileExtensions_
    }

-- | The character set encoding of the data set.
poAttributes_encoding :: Lens.Lens' PoAttributes (Prelude.Maybe Prelude.Text)
poAttributes_encoding = Lens.lens (\PoAttributes' {encoding} -> encoding) (\s@PoAttributes' {} a -> s {encoding = a} :: PoAttributes)

-- | The format of the data set records.
poAttributes_format :: Lens.Lens' PoAttributes Prelude.Text
poAttributes_format = Lens.lens (\PoAttributes' {format} -> format) (\s@PoAttributes' {} a -> s {format = a} :: PoAttributes)

-- | An array containing one or more filename extensions, allowing you to
-- specify which files to be included as PDS member.
poAttributes_memberFileExtensions :: Lens.Lens' PoAttributes (Prelude.NonEmpty Prelude.Text)
poAttributes_memberFileExtensions = Lens.lens (\PoAttributes' {memberFileExtensions} -> memberFileExtensions) (\s@PoAttributes' {} a -> s {memberFileExtensions = a} :: PoAttributes) Prelude.. Lens.coerced

instance Prelude.Hashable PoAttributes where
  hashWithSalt _salt PoAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` memberFileExtensions

instance Prelude.NFData PoAttributes where
  rnf PoAttributes' {..} =
    Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf memberFileExtensions

instance Data.ToJSON PoAttributes where
  toJSON PoAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encoding" Data..=) Prelude.<$> encoding,
            Prelude.Just ("format" Data..= format),
            Prelude.Just
              ( "memberFileExtensions"
                  Data..= memberFileExtensions
              )
          ]
      )
