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
-- Module      : Network.AWS.Config.Types.StaticValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StaticValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The static value of the resource.
--
-- /See:/ 'newStaticValue' smart constructor.
data StaticValue = StaticValue'
  { -- | A list of values. For example, the ARN of the assumed role.
    values :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StaticValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'staticValue_values' - A list of values. For example, the ARN of the assumed role.
newStaticValue ::
  StaticValue
newStaticValue = StaticValue' {values = Core.mempty}

-- | A list of values. For example, the ARN of the assumed role.
staticValue_values :: Lens.Lens' StaticValue [Core.Text]
staticValue_values = Lens.lens (\StaticValue' {values} -> values) (\s@StaticValue' {} a -> s {values = a} :: StaticValue) Core.. Lens._Coerce

instance Core.FromJSON StaticValue where
  parseJSON =
    Core.withObject
      "StaticValue"
      ( \x ->
          StaticValue'
            Core.<$> (x Core..:? "Values" Core..!= Core.mempty)
      )

instance Core.Hashable StaticValue

instance Core.NFData StaticValue

instance Core.ToJSON StaticValue where
  toJSON StaticValue' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Values" Core..= values)]
      )
