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
-- Module      : Amazonka.Lambda.Types.AliasRoutingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.AliasRoutingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html traffic-shifting>
-- configuration of a Lambda function alias.
--
-- /See:/ 'newAliasRoutingConfiguration' smart constructor.
data AliasRoutingConfiguration = AliasRoutingConfiguration'
  { -- | The second version, and the percentage of traffic that\'s routed to it.
    additionalVersionWeights :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AliasRoutingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalVersionWeights', 'aliasRoutingConfiguration_additionalVersionWeights' - The second version, and the percentage of traffic that\'s routed to it.
newAliasRoutingConfiguration ::
  AliasRoutingConfiguration
newAliasRoutingConfiguration =
  AliasRoutingConfiguration'
    { additionalVersionWeights =
        Prelude.Nothing
    }

-- | The second version, and the percentage of traffic that\'s routed to it.
aliasRoutingConfiguration_additionalVersionWeights :: Lens.Lens' AliasRoutingConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
aliasRoutingConfiguration_additionalVersionWeights = Lens.lens (\AliasRoutingConfiguration' {additionalVersionWeights} -> additionalVersionWeights) (\s@AliasRoutingConfiguration' {} a -> s {additionalVersionWeights = a} :: AliasRoutingConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AliasRoutingConfiguration where
  parseJSON =
    Data.withObject
      "AliasRoutingConfiguration"
      ( \x ->
          AliasRoutingConfiguration'
            Prelude.<$> ( x Data..:? "AdditionalVersionWeights"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AliasRoutingConfiguration where
  hashWithSalt _salt AliasRoutingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` additionalVersionWeights

instance Prelude.NFData AliasRoutingConfiguration where
  rnf AliasRoutingConfiguration' {..} =
    Prelude.rnf additionalVersionWeights

instance Data.ToJSON AliasRoutingConfiguration where
  toJSON AliasRoutingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalVersionWeights" Data..=)
              Prelude.<$> additionalVersionWeights
          ]
      )
