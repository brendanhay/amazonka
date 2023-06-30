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
-- Module      : Amazonka.IotTwinMaker.Types.DataConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.DataConnector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.LambdaFunction
import qualified Amazonka.Prelude as Prelude

-- | The data connector.
--
-- /See:/ 'newDataConnector' smart constructor.
data DataConnector = DataConnector'
  { -- | A Boolean value that specifies whether the data connector is native to
    -- IoT TwinMaker.
    isNative :: Prelude.Maybe Prelude.Bool,
    -- | The Lambda function associated with this data connector.
    lambda :: Prelude.Maybe LambdaFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isNative', 'dataConnector_isNative' - A Boolean value that specifies whether the data connector is native to
-- IoT TwinMaker.
--
-- 'lambda', 'dataConnector_lambda' - The Lambda function associated with this data connector.
newDataConnector ::
  DataConnector
newDataConnector =
  DataConnector'
    { isNative = Prelude.Nothing,
      lambda = Prelude.Nothing
    }

-- | A Boolean value that specifies whether the data connector is native to
-- IoT TwinMaker.
dataConnector_isNative :: Lens.Lens' DataConnector (Prelude.Maybe Prelude.Bool)
dataConnector_isNative = Lens.lens (\DataConnector' {isNative} -> isNative) (\s@DataConnector' {} a -> s {isNative = a} :: DataConnector)

-- | The Lambda function associated with this data connector.
dataConnector_lambda :: Lens.Lens' DataConnector (Prelude.Maybe LambdaFunction)
dataConnector_lambda = Lens.lens (\DataConnector' {lambda} -> lambda) (\s@DataConnector' {} a -> s {lambda = a} :: DataConnector)

instance Data.FromJSON DataConnector where
  parseJSON =
    Data.withObject
      "DataConnector"
      ( \x ->
          DataConnector'
            Prelude.<$> (x Data..:? "isNative")
            Prelude.<*> (x Data..:? "lambda")
      )

instance Prelude.Hashable DataConnector where
  hashWithSalt _salt DataConnector' {..} =
    _salt
      `Prelude.hashWithSalt` isNative
      `Prelude.hashWithSalt` lambda

instance Prelude.NFData DataConnector where
  rnf DataConnector' {..} =
    Prelude.rnf isNative
      `Prelude.seq` Prelude.rnf lambda

instance Data.ToJSON DataConnector where
  toJSON DataConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isNative" Data..=) Prelude.<$> isNative,
            ("lambda" Data..=) Prelude.<$> lambda
          ]
      )
