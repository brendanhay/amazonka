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
-- Module      : Amazonka.Route53.Types.Dimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.Dimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | For the metric that the CloudWatch alarm is associated with, a complex
-- type that contains information about one dimension.
--
-- /See:/ 'newDimension' smart constructor.
data Dimension = Dimension'
  { -- | For the metric that the CloudWatch alarm is associated with, the name of
    -- one dimension.
    name :: Prelude.Text,
    -- | For the metric that the CloudWatch alarm is associated with, the value
    -- of one dimension.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dimension_name' - For the metric that the CloudWatch alarm is associated with, the name of
-- one dimension.
--
-- 'value', 'dimension_value' - For the metric that the CloudWatch alarm is associated with, the value
-- of one dimension.
newDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Dimension
newDimension pName_ pValue_ =
  Dimension' {name = pName_, value = pValue_}

-- | For the metric that the CloudWatch alarm is associated with, the name of
-- one dimension.
dimension_name :: Lens.Lens' Dimension Prelude.Text
dimension_name = Lens.lens (\Dimension' {name} -> name) (\s@Dimension' {} a -> s {name = a} :: Dimension)

-- | For the metric that the CloudWatch alarm is associated with, the value
-- of one dimension.
dimension_value :: Lens.Lens' Dimension Prelude.Text
dimension_value = Lens.lens (\Dimension' {value} -> value) (\s@Dimension' {} a -> s {value = a} :: Dimension)

instance Data.FromXML Dimension where
  parseXML x =
    Dimension'
      Prelude.<$> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "Value")

instance Prelude.Hashable Dimension where
  hashWithSalt _salt Dimension' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Dimension where
  rnf Dimension' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
