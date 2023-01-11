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
-- Module      : Amazonka.Rekognition.Types.LabelAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LabelAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A potential alias of for a given label.
--
-- /See:/ 'newLabelAlias' smart constructor.
data LabelAlias = LabelAlias'
  { -- | The name of an alias for a given label.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'labelAlias_name' - The name of an alias for a given label.
newLabelAlias ::
  LabelAlias
newLabelAlias = LabelAlias' {name = Prelude.Nothing}

-- | The name of an alias for a given label.
labelAlias_name :: Lens.Lens' LabelAlias (Prelude.Maybe Prelude.Text)
labelAlias_name = Lens.lens (\LabelAlias' {name} -> name) (\s@LabelAlias' {} a -> s {name = a} :: LabelAlias)

instance Data.FromJSON LabelAlias where
  parseJSON =
    Data.withObject
      "LabelAlias"
      (\x -> LabelAlias' Prelude.<$> (x Data..:? "Name"))

instance Prelude.Hashable LabelAlias where
  hashWithSalt _salt LabelAlias' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData LabelAlias where
  rnf LabelAlias' {..} = Prelude.rnf name
