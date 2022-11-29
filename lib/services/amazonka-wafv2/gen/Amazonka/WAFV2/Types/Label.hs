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
-- Module      : Amazonka.WAFV2.Types.Label
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Label where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A single label container. This is used as an element of a label array in
-- multiple contexts, for example, in @RuleLabels@ inside a Rule and in
-- @Labels@ inside a SampledHTTPRequest.
--
-- /See:/ 'newLabel' smart constructor.
data Label = Label'
  { -- | The label string.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Label' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'label_name' - The label string.
newLabel ::
  -- | 'name'
  Prelude.Text ->
  Label
newLabel pName_ = Label' {name = pName_}

-- | The label string.
label_name :: Lens.Lens' Label Prelude.Text
label_name = Lens.lens (\Label' {name} -> name) (\s@Label' {} a -> s {name = a} :: Label)

instance Core.FromJSON Label where
  parseJSON =
    Core.withObject
      "Label"
      (\x -> Label' Prelude.<$> (x Core..: "Name"))

instance Prelude.Hashable Label where
  hashWithSalt _salt Label' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData Label where
  rnf Label' {..} = Prelude.rnf name

instance Core.ToJSON Label where
  toJSON Label' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )
