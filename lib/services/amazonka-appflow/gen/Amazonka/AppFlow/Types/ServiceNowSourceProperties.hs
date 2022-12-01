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
-- Module      : Amazonka.AppFlow.Types.ServiceNowSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ServiceNowSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when ServiceNow is being used as a
-- source.
--
-- /See:/ 'newServiceNowSourceProperties' smart constructor.
data ServiceNowSourceProperties = ServiceNowSourceProperties'
  { -- | The object specified in the ServiceNow flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'serviceNowSourceProperties_object' - The object specified in the ServiceNow flow source.
newServiceNowSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  ServiceNowSourceProperties
newServiceNowSourceProperties pObject_ =
  ServiceNowSourceProperties' {object' = pObject_}

-- | The object specified in the ServiceNow flow source.
serviceNowSourceProperties_object :: Lens.Lens' ServiceNowSourceProperties Prelude.Text
serviceNowSourceProperties_object = Lens.lens (\ServiceNowSourceProperties' {object'} -> object') (\s@ServiceNowSourceProperties' {} a -> s {object' = a} :: ServiceNowSourceProperties)

instance Core.FromJSON ServiceNowSourceProperties where
  parseJSON =
    Core.withObject
      "ServiceNowSourceProperties"
      ( \x ->
          ServiceNowSourceProperties'
            Prelude.<$> (x Core..: "object")
      )

instance Prelude.Hashable ServiceNowSourceProperties where
  hashWithSalt _salt ServiceNowSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData ServiceNowSourceProperties where
  rnf ServiceNowSourceProperties' {..} =
    Prelude.rnf object'

instance Core.ToJSON ServiceNowSourceProperties where
  toJSON ServiceNowSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Core..= object')]
      )
