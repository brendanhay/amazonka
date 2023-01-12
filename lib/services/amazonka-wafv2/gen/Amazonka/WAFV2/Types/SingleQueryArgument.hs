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
-- Module      : Amazonka.WAFV2.Types.SingleQueryArgument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.SingleQueryArgument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Inspect one query argument in the web request, identified by name, for
-- example /UserName/ or /SalesRegion/. The name isn\'t case sensitive.
--
-- This is used to indicate the web request component to inspect, in the
-- FieldToMatch specification.
--
-- Example JSON: @\"SingleQueryArgument\": { \"Name\": \"myArgument\" }@
--
-- /See:/ 'newSingleQueryArgument' smart constructor.
data SingleQueryArgument = SingleQueryArgument'
  { -- | The name of the query argument to inspect.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleQueryArgument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'singleQueryArgument_name' - The name of the query argument to inspect.
newSingleQueryArgument ::
  -- | 'name'
  Prelude.Text ->
  SingleQueryArgument
newSingleQueryArgument pName_ =
  SingleQueryArgument' {name = pName_}

-- | The name of the query argument to inspect.
singleQueryArgument_name :: Lens.Lens' SingleQueryArgument Prelude.Text
singleQueryArgument_name = Lens.lens (\SingleQueryArgument' {name} -> name) (\s@SingleQueryArgument' {} a -> s {name = a} :: SingleQueryArgument)

instance Data.FromJSON SingleQueryArgument where
  parseJSON =
    Data.withObject
      "SingleQueryArgument"
      ( \x ->
          SingleQueryArgument' Prelude.<$> (x Data..: "Name")
      )

instance Prelude.Hashable SingleQueryArgument where
  hashWithSalt _salt SingleQueryArgument' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData SingleQueryArgument where
  rnf SingleQueryArgument' {..} = Prelude.rnf name

instance Data.ToJSON SingleQueryArgument where
  toJSON SingleQueryArgument' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )
