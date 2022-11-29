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
-- Module      : Amazonka.WAFV2.Types.SingleHeader
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.SingleHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Inspect one of the headers in the web request, identified by name, for
-- example, @User-Agent@ or @Referer@. The name isn\'t case sensitive.
--
-- You can filter and inspect all headers with the @FieldToMatch@ setting
-- @Headers@.
--
-- This is used to indicate the web request component to inspect, in the
-- FieldToMatch specification.
--
-- Example JSON: @\"SingleHeader\": { \"Name\": \"haystack\" }@
--
-- /See:/ 'newSingleHeader' smart constructor.
data SingleHeader = SingleHeader'
  { -- | The name of the query header to inspect.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'singleHeader_name' - The name of the query header to inspect.
newSingleHeader ::
  -- | 'name'
  Prelude.Text ->
  SingleHeader
newSingleHeader pName_ = SingleHeader' {name = pName_}

-- | The name of the query header to inspect.
singleHeader_name :: Lens.Lens' SingleHeader Prelude.Text
singleHeader_name = Lens.lens (\SingleHeader' {name} -> name) (\s@SingleHeader' {} a -> s {name = a} :: SingleHeader)

instance Core.FromJSON SingleHeader where
  parseJSON =
    Core.withObject
      "SingleHeader"
      (\x -> SingleHeader' Prelude.<$> (x Core..: "Name"))

instance Prelude.Hashable SingleHeader where
  hashWithSalt _salt SingleHeader' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData SingleHeader where
  rnf SingleHeader' {..} = Prelude.rnf name

instance Core.ToJSON SingleHeader where
  toJSON SingleHeader' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )
