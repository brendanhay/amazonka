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
-- Module      : Network.AWS.CloudWatch.Types.Dimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Dimension where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A dimension is a name\/value pair that is part of the identity of a
-- metric. You can assign up to 10 dimensions to a metric. Because
-- dimensions are part of the unique identifier for a metric, whenever you
-- add a unique name\/value pair to one of your metrics, you are creating a
-- new variation of that metric.
--
-- /See:/ 'newDimension' smart constructor.
data Dimension = Dimension'
  { -- | The name of the dimension. Dimension names cannot contain blank spaces
    -- or non-ASCII characters.
    name :: Core.Text,
    -- | The value of the dimension. Dimension values cannot contain blank spaces
    -- or non-ASCII characters.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Dimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dimension_name' - The name of the dimension. Dimension names cannot contain blank spaces
-- or non-ASCII characters.
--
-- 'value', 'dimension_value' - The value of the dimension. Dimension values cannot contain blank spaces
-- or non-ASCII characters.
newDimension ::
  -- | 'name'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  Dimension
newDimension pName_ pValue_ =
  Dimension' {name = pName_, value = pValue_}

-- | The name of the dimension. Dimension names cannot contain blank spaces
-- or non-ASCII characters.
dimension_name :: Lens.Lens' Dimension Core.Text
dimension_name = Lens.lens (\Dimension' {name} -> name) (\s@Dimension' {} a -> s {name = a} :: Dimension)

-- | The value of the dimension. Dimension values cannot contain blank spaces
-- or non-ASCII characters.
dimension_value :: Lens.Lens' Dimension Core.Text
dimension_value = Lens.lens (\Dimension' {value} -> value) (\s@Dimension' {} a -> s {value = a} :: Dimension)

instance Core.FromXML Dimension where
  parseXML x =
    Dimension'
      Core.<$> (x Core..@ "Name") Core.<*> (x Core..@ "Value")

instance Core.Hashable Dimension

instance Core.NFData Dimension

instance Core.ToQuery Dimension where
  toQuery Dimension' {..} =
    Core.mconcat
      ["Name" Core.=: name, "Value" Core.=: value]
