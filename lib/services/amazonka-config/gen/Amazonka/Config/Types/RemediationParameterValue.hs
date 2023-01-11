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
-- Module      : Amazonka.Config.Types.RemediationParameterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationParameterValue where

import Amazonka.Config.Types.ResourceValue
import Amazonka.Config.Types.StaticValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value is either a dynamic (resource) value or a static value. You
-- must select either a dynamic value or a static value.
--
-- /See:/ 'newRemediationParameterValue' smart constructor.
data RemediationParameterValue = RemediationParameterValue'
  { -- | The value is dynamic and changes at run-time.
    resourceValue :: Prelude.Maybe ResourceValue,
    -- | The value is static and does not change at run-time.
    staticValue :: Prelude.Maybe StaticValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceValue', 'remediationParameterValue_resourceValue' - The value is dynamic and changes at run-time.
--
-- 'staticValue', 'remediationParameterValue_staticValue' - The value is static and does not change at run-time.
newRemediationParameterValue ::
  RemediationParameterValue
newRemediationParameterValue =
  RemediationParameterValue'
    { resourceValue =
        Prelude.Nothing,
      staticValue = Prelude.Nothing
    }

-- | The value is dynamic and changes at run-time.
remediationParameterValue_resourceValue :: Lens.Lens' RemediationParameterValue (Prelude.Maybe ResourceValue)
remediationParameterValue_resourceValue = Lens.lens (\RemediationParameterValue' {resourceValue} -> resourceValue) (\s@RemediationParameterValue' {} a -> s {resourceValue = a} :: RemediationParameterValue)

-- | The value is static and does not change at run-time.
remediationParameterValue_staticValue :: Lens.Lens' RemediationParameterValue (Prelude.Maybe StaticValue)
remediationParameterValue_staticValue = Lens.lens (\RemediationParameterValue' {staticValue} -> staticValue) (\s@RemediationParameterValue' {} a -> s {staticValue = a} :: RemediationParameterValue)

instance Data.FromJSON RemediationParameterValue where
  parseJSON =
    Data.withObject
      "RemediationParameterValue"
      ( \x ->
          RemediationParameterValue'
            Prelude.<$> (x Data..:? "ResourceValue")
            Prelude.<*> (x Data..:? "StaticValue")
      )

instance Prelude.Hashable RemediationParameterValue where
  hashWithSalt _salt RemediationParameterValue' {..} =
    _salt `Prelude.hashWithSalt` resourceValue
      `Prelude.hashWithSalt` staticValue

instance Prelude.NFData RemediationParameterValue where
  rnf RemediationParameterValue' {..} =
    Prelude.rnf resourceValue
      `Prelude.seq` Prelude.rnf staticValue

instance Data.ToJSON RemediationParameterValue where
  toJSON RemediationParameterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceValue" Data..=) Prelude.<$> resourceValue,
            ("StaticValue" Data..=) Prelude.<$> staticValue
          ]
      )
