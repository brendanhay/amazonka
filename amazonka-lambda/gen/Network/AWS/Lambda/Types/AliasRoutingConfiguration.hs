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
-- Module      : Network.AWS.Lambda.Types.AliasRoutingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AliasRoutingConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html traffic-shifting>
-- configuration of a Lambda function alias.
--
-- /See:/ 'newAliasRoutingConfiguration' smart constructor.
data AliasRoutingConfiguration = AliasRoutingConfiguration'
  { -- | The second version, and the percentage of traffic that\'s routed to it.
    additionalVersionWeights :: Core.Maybe (Core.HashMap Core.Text Core.Double)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The second version, and the percentage of traffic that\'s routed to it.
aliasRoutingConfiguration_additionalVersionWeights :: Lens.Lens' AliasRoutingConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Double))
aliasRoutingConfiguration_additionalVersionWeights = Lens.lens (\AliasRoutingConfiguration' {additionalVersionWeights} -> additionalVersionWeights) (\s@AliasRoutingConfiguration' {} a -> s {additionalVersionWeights = a} :: AliasRoutingConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AliasRoutingConfiguration where
  parseJSON =
    Core.withObject
      "AliasRoutingConfiguration"
      ( \x ->
          AliasRoutingConfiguration'
            Core.<$> ( x Core..:? "AdditionalVersionWeights"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable AliasRoutingConfiguration

instance Core.NFData AliasRoutingConfiguration

instance Core.ToJSON AliasRoutingConfiguration where
  toJSON AliasRoutingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AdditionalVersionWeights" Core..=)
              Core.<$> additionalVersionWeights
          ]
      )
