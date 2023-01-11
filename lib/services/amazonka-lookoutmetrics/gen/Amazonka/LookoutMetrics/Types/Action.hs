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
-- Module      : Amazonka.LookoutMetrics.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.LambdaConfiguration
import Amazonka.LookoutMetrics.Types.SNSConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A configuration that specifies the action to perform when anomalies are
-- detected.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | A configuration for an AWS Lambda channel.
    lambdaConfiguration :: Prelude.Maybe LambdaConfiguration,
    -- | A configuration for an Amazon SNS channel.
    sNSConfiguration :: Prelude.Maybe SNSConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaConfiguration', 'action_lambdaConfiguration' - A configuration for an AWS Lambda channel.
--
-- 'sNSConfiguration', 'action_sNSConfiguration' - A configuration for an Amazon SNS channel.
newAction ::
  Action
newAction =
  Action'
    { lambdaConfiguration = Prelude.Nothing,
      sNSConfiguration = Prelude.Nothing
    }

-- | A configuration for an AWS Lambda channel.
action_lambdaConfiguration :: Lens.Lens' Action (Prelude.Maybe LambdaConfiguration)
action_lambdaConfiguration = Lens.lens (\Action' {lambdaConfiguration} -> lambdaConfiguration) (\s@Action' {} a -> s {lambdaConfiguration = a} :: Action)

-- | A configuration for an Amazon SNS channel.
action_sNSConfiguration :: Lens.Lens' Action (Prelude.Maybe SNSConfiguration)
action_sNSConfiguration = Lens.lens (\Action' {sNSConfiguration} -> sNSConfiguration) (\s@Action' {} a -> s {sNSConfiguration = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "LambdaConfiguration")
            Prelude.<*> (x Data..:? "SNSConfiguration")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt `Prelude.hashWithSalt` lambdaConfiguration
      `Prelude.hashWithSalt` sNSConfiguration

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf lambdaConfiguration
      `Prelude.seq` Prelude.rnf sNSConfiguration

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LambdaConfiguration" Data..=)
              Prelude.<$> lambdaConfiguration,
            ("SNSConfiguration" Data..=)
              Prelude.<$> sNSConfiguration
          ]
      )
