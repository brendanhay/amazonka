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
-- Module      : Amazonka.WAFV2.Types.AllQueryArguments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AllQueryArguments where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Inspect all query arguments of the web request.
--
-- This is used in the FieldToMatch specification for some web request
-- component types.
--
-- JSON specification: @\"AllQueryArguments\": {}@
--
-- /See:/ 'newAllQueryArguments' smart constructor.
data AllQueryArguments = AllQueryArguments'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllQueryArguments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAllQueryArguments ::
  AllQueryArguments
newAllQueryArguments = AllQueryArguments'

instance Data.FromJSON AllQueryArguments where
  parseJSON =
    Data.withObject
      "AllQueryArguments"
      (\x -> Prelude.pure AllQueryArguments')

instance Prelude.Hashable AllQueryArguments where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData AllQueryArguments where
  rnf _ = ()

instance Data.ToJSON AllQueryArguments where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
