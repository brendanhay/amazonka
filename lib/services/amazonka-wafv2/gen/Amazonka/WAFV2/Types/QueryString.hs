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
-- Module      : Amazonka.WAFV2.Types.QueryString
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.QueryString where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Inspect the query string of the web request. This is the part of a URL
-- that appears after a @?@ character, if any.
--
-- This is used only in the FieldToMatch specification for some web request
-- component types.
--
-- JSON specification: @\"QueryString\": {}@
--
-- /See:/ 'newQueryString' smart constructor.
data QueryString = QueryString'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryString' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newQueryString ::
  QueryString
newQueryString = QueryString'

instance Data.FromJSON QueryString where
  parseJSON =
    Data.withObject
      "QueryString"
      (\x -> Prelude.pure QueryString')

instance Prelude.Hashable QueryString where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData QueryString where
  rnf _ = ()

instance Data.ToJSON QueryString where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
