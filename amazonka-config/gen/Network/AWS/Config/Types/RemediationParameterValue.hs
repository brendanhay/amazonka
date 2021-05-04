{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.RemediationParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationParameterValue where

import Network.AWS.Config.Types.ResourceValue
import Network.AWS.Config.Types.StaticValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON RemediationParameterValue where
  parseJSON =
    Prelude.withObject
      "RemediationParameterValue"
      ( \x ->
          RemediationParameterValue'
            Prelude.<$> (x Prelude..:? "ResourceValue")
            Prelude.<*> (x Prelude..:? "StaticValue")
      )

instance Prelude.Hashable RemediationParameterValue

instance Prelude.NFData RemediationParameterValue

instance Prelude.ToJSON RemediationParameterValue where
  toJSON RemediationParameterValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceValue" Prelude..=)
              Prelude.<$> resourceValue,
            ("StaticValue" Prelude..=) Prelude.<$> staticValue
          ]
      )
