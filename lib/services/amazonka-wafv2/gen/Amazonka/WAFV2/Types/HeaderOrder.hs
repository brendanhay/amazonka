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
-- Module      : Amazonka.WAFV2.Types.HeaderOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.HeaderOrder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.OversizeHandling

-- | Inspect a string containing the list of the request\'s header names,
-- ordered as they appear in the web request that WAF receives for
-- inspection. WAF generates the string and then uses that as the field to
-- match component in its inspection. WAF separates the header names in the
-- string using colons and no added spaces, for example
-- @host:user-agent:accept:authorization:referer@.
--
-- /See:/ 'newHeaderOrder' smart constructor.
data HeaderOrder = HeaderOrder'
  { -- | What WAF should do if the headers of the request are more numerous or
    -- larger than WAF can inspect. WAF does not support inspecting the entire
    -- contents of request headers when they exceed 8 KB (8192 bytes) or 200
    -- total headers. The underlying host service forwards a maximum of 200
    -- headers and at most 8 KB of header contents to WAF.
    --
    -- The options for oversize handling are the following:
    --
    -- -   @CONTINUE@ - Inspect the available headers normally, according to
    --     the rule inspection criteria.
    --
    -- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
    --     applies the rule action to the request.
    --
    -- -   @NO_MATCH@ - Treat the web request as not matching the rule
    --     statement.
    oversizeHandling :: OversizeHandling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeaderOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oversizeHandling', 'headerOrder_oversizeHandling' - What WAF should do if the headers of the request are more numerous or
-- larger than WAF can inspect. WAF does not support inspecting the entire
-- contents of request headers when they exceed 8 KB (8192 bytes) or 200
-- total headers. The underlying host service forwards a maximum of 200
-- headers and at most 8 KB of header contents to WAF.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the available headers normally, according to
--     the rule inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
newHeaderOrder ::
  -- | 'oversizeHandling'
  OversizeHandling ->
  HeaderOrder
newHeaderOrder pOversizeHandling_ =
  HeaderOrder' {oversizeHandling = pOversizeHandling_}

-- | What WAF should do if the headers of the request are more numerous or
-- larger than WAF can inspect. WAF does not support inspecting the entire
-- contents of request headers when they exceed 8 KB (8192 bytes) or 200
-- total headers. The underlying host service forwards a maximum of 200
-- headers and at most 8 KB of header contents to WAF.
--
-- The options for oversize handling are the following:
--
-- -   @CONTINUE@ - Inspect the available headers normally, according to
--     the rule inspection criteria.
--
-- -   @MATCH@ - Treat the web request as matching the rule statement. WAF
--     applies the rule action to the request.
--
-- -   @NO_MATCH@ - Treat the web request as not matching the rule
--     statement.
headerOrder_oversizeHandling :: Lens.Lens' HeaderOrder OversizeHandling
headerOrder_oversizeHandling = Lens.lens (\HeaderOrder' {oversizeHandling} -> oversizeHandling) (\s@HeaderOrder' {} a -> s {oversizeHandling = a} :: HeaderOrder)

instance Data.FromJSON HeaderOrder where
  parseJSON =
    Data.withObject
      "HeaderOrder"
      ( \x ->
          HeaderOrder'
            Prelude.<$> (x Data..: "OversizeHandling")
      )

instance Prelude.Hashable HeaderOrder where
  hashWithSalt _salt HeaderOrder' {..} =
    _salt `Prelude.hashWithSalt` oversizeHandling

instance Prelude.NFData HeaderOrder where
  rnf HeaderOrder' {..} = Prelude.rnf oversizeHandling

instance Data.ToJSON HeaderOrder where
  toJSON HeaderOrder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OversizeHandling" Data..= oversizeHandling)
          ]
      )
