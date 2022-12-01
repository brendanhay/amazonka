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
-- Module      : Amazonka.AppFlow.Types.MarketoSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.MarketoSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Marketo is being used as a source.
--
-- /See:/ 'newMarketoSourceProperties' smart constructor.
data MarketoSourceProperties = MarketoSourceProperties'
  { -- | The object specified in the Marketo flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MarketoSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'marketoSourceProperties_object' - The object specified in the Marketo flow source.
newMarketoSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  MarketoSourceProperties
newMarketoSourceProperties pObject_ =
  MarketoSourceProperties' {object' = pObject_}

-- | The object specified in the Marketo flow source.
marketoSourceProperties_object :: Lens.Lens' MarketoSourceProperties Prelude.Text
marketoSourceProperties_object = Lens.lens (\MarketoSourceProperties' {object'} -> object') (\s@MarketoSourceProperties' {} a -> s {object' = a} :: MarketoSourceProperties)

instance Core.FromJSON MarketoSourceProperties where
  parseJSON =
    Core.withObject
      "MarketoSourceProperties"
      ( \x ->
          MarketoSourceProperties'
            Prelude.<$> (x Core..: "object")
      )

instance Prelude.Hashable MarketoSourceProperties where
  hashWithSalt _salt MarketoSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData MarketoSourceProperties where
  rnf MarketoSourceProperties' {..} =
    Prelude.rnf object'

instance Core.ToJSON MarketoSourceProperties where
  toJSON MarketoSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Core..= object')]
      )
