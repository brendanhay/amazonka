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
-- Module      : Network.AWS.ComprehendMedical.Types.Trait
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComprehendMedical.Types.Trait where

import Network.AWS.ComprehendMedical.Types.AttributeName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides contextual information about the extracted entity.
--
-- /See:/ 'newTrait' smart constructor.
data Trait = Trait'
  { -- | The level of confidence that Amazon Comprehend Medical has in the
    -- accuracy of this trait.
    score :: Prelude.Maybe Prelude.Double,
    -- | Provides a name or contextual description about the trait.
    name :: Prelude.Maybe AttributeName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trait' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'trait_score' - The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of this trait.
--
-- 'name', 'trait_name' - Provides a name or contextual description about the trait.
newTrait ::
  Trait
newTrait =
  Trait'
    { score = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of this trait.
trait_score :: Lens.Lens' Trait (Prelude.Maybe Prelude.Double)
trait_score = Lens.lens (\Trait' {score} -> score) (\s@Trait' {} a -> s {score = a} :: Trait)

-- | Provides a name or contextual description about the trait.
trait_name :: Lens.Lens' Trait (Prelude.Maybe AttributeName)
trait_name = Lens.lens (\Trait' {name} -> name) (\s@Trait' {} a -> s {name = a} :: Trait)

instance Core.FromJSON Trait where
  parseJSON =
    Core.withObject
      "Trait"
      ( \x ->
          Trait'
            Prelude.<$> (x Core..:? "Score") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable Trait

instance Prelude.NFData Trait
