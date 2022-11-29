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
-- Module      : Amazonka.AppFlow.Types.DatadogSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DatadogSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Datadog is being used as a source.
--
-- /See:/ 'newDatadogSourceProperties' smart constructor.
data DatadogSourceProperties = DatadogSourceProperties'
  { -- | The object specified in the Datadog flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatadogSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'datadogSourceProperties_object' - The object specified in the Datadog flow source.
newDatadogSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  DatadogSourceProperties
newDatadogSourceProperties pObject_ =
  DatadogSourceProperties' {object' = pObject_}

-- | The object specified in the Datadog flow source.
datadogSourceProperties_object :: Lens.Lens' DatadogSourceProperties Prelude.Text
datadogSourceProperties_object = Lens.lens (\DatadogSourceProperties' {object'} -> object') (\s@DatadogSourceProperties' {} a -> s {object' = a} :: DatadogSourceProperties)

instance Core.FromJSON DatadogSourceProperties where
  parseJSON =
    Core.withObject
      "DatadogSourceProperties"
      ( \x ->
          DatadogSourceProperties'
            Prelude.<$> (x Core..: "object")
      )

instance Prelude.Hashable DatadogSourceProperties where
  hashWithSalt _salt DatadogSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData DatadogSourceProperties where
  rnf DatadogSourceProperties' {..} =
    Prelude.rnf object'

instance Core.ToJSON DatadogSourceProperties where
  toJSON DatadogSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Core..= object')]
      )
