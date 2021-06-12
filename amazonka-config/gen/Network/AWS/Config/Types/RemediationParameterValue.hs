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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The value is either a dynamic (resource) value or a static value. You
-- must select either a dynamic value or a static value.
--
-- /See:/ 'newRemediationParameterValue' smart constructor.
data RemediationParameterValue = RemediationParameterValue'
  { -- | The value is dynamic and changes at run-time.
    resourceValue :: Core.Maybe ResourceValue,
    -- | The value is static and does not change at run-time.
    staticValue :: Core.Maybe StaticValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      staticValue = Core.Nothing
    }

-- | The value is dynamic and changes at run-time.
remediationParameterValue_resourceValue :: Lens.Lens' RemediationParameterValue (Core.Maybe ResourceValue)
remediationParameterValue_resourceValue = Lens.lens (\RemediationParameterValue' {resourceValue} -> resourceValue) (\s@RemediationParameterValue' {} a -> s {resourceValue = a} :: RemediationParameterValue)

-- | The value is static and does not change at run-time.
remediationParameterValue_staticValue :: Lens.Lens' RemediationParameterValue (Core.Maybe StaticValue)
remediationParameterValue_staticValue = Lens.lens (\RemediationParameterValue' {staticValue} -> staticValue) (\s@RemediationParameterValue' {} a -> s {staticValue = a} :: RemediationParameterValue)

instance Core.FromJSON RemediationParameterValue where
  parseJSON =
    Core.withObject
      "RemediationParameterValue"
      ( \x ->
          RemediationParameterValue'
            Core.<$> (x Core..:? "ResourceValue")
            Core.<*> (x Core..:? "StaticValue")
      )

instance Core.Hashable RemediationParameterValue

instance Core.NFData RemediationParameterValue

instance Core.ToJSON RemediationParameterValue where
  toJSON RemediationParameterValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceValue" Core..=) Core.<$> resourceValue,
            ("StaticValue" Core..=) Core.<$> staticValue
          ]
      )
