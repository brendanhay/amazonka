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
-- Module      : Amazonka.AppFlow.Types.TrendmicroConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.TrendmicroConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using Trend
-- Micro.
--
-- /See:/ 'newTrendmicroConnectorProfileProperties' smart constructor.
data TrendmicroConnectorProfileProperties = TrendmicroConnectorProfileProperties'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrendmicroConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTrendmicroConnectorProfileProperties ::
  TrendmicroConnectorProfileProperties
newTrendmicroConnectorProfileProperties =
  TrendmicroConnectorProfileProperties'

instance
  Data.FromJSON
    TrendmicroConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "TrendmicroConnectorProfileProperties"
      ( \x ->
          Prelude.pure TrendmicroConnectorProfileProperties'
      )

instance
  Prelude.Hashable
    TrendmicroConnectorProfileProperties
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    TrendmicroConnectorProfileProperties
  where
  rnf _ = ()

instance
  Data.ToJSON
    TrendmicroConnectorProfileProperties
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
