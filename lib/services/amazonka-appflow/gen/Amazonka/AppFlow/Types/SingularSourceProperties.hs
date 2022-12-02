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
-- Module      : Amazonka.AppFlow.Types.SingularSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SingularSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Singular is being used as a source.
--
-- /See:/ 'newSingularSourceProperties' smart constructor.
data SingularSourceProperties = SingularSourceProperties'
  { -- | The object specified in the Singular flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingularSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'singularSourceProperties_object' - The object specified in the Singular flow source.
newSingularSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  SingularSourceProperties
newSingularSourceProperties pObject_ =
  SingularSourceProperties' {object' = pObject_}

-- | The object specified in the Singular flow source.
singularSourceProperties_object :: Lens.Lens' SingularSourceProperties Prelude.Text
singularSourceProperties_object = Lens.lens (\SingularSourceProperties' {object'} -> object') (\s@SingularSourceProperties' {} a -> s {object' = a} :: SingularSourceProperties)

instance Data.FromJSON SingularSourceProperties where
  parseJSON =
    Data.withObject
      "SingularSourceProperties"
      ( \x ->
          SingularSourceProperties'
            Prelude.<$> (x Data..: "object")
      )

instance Prelude.Hashable SingularSourceProperties where
  hashWithSalt _salt SingularSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData SingularSourceProperties where
  rnf SingularSourceProperties' {..} =
    Prelude.rnf object'

instance Data.ToJSON SingularSourceProperties where
  toJSON SingularSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Data..= object')]
      )
