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
-- Module      : Amazonka.Rekognition.Types.KnownGender
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.KnownGender where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.KnownGenderType

-- | The known gender identity for the celebrity that matches the provided
-- ID. The known gender identity can be Male, Female, Nonbinary, or
-- Unlisted.
--
-- /See:/ 'newKnownGender' smart constructor.
data KnownGender = KnownGender'
  { -- | A string value of the KnownGender info about the Celebrity.
    type' :: Prelude.Maybe KnownGenderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KnownGender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'knownGender_type' - A string value of the KnownGender info about the Celebrity.
newKnownGender ::
  KnownGender
newKnownGender =
  KnownGender' {type' = Prelude.Nothing}

-- | A string value of the KnownGender info about the Celebrity.
knownGender_type :: Lens.Lens' KnownGender (Prelude.Maybe KnownGenderType)
knownGender_type = Lens.lens (\KnownGender' {type'} -> type') (\s@KnownGender' {} a -> s {type' = a} :: KnownGender)

instance Data.FromJSON KnownGender where
  parseJSON =
    Data.withObject
      "KnownGender"
      (\x -> KnownGender' Prelude.<$> (x Data..:? "Type"))

instance Prelude.Hashable KnownGender where
  hashWithSalt _salt KnownGender' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData KnownGender where
  rnf KnownGender' {..} = Prelude.rnf type'
