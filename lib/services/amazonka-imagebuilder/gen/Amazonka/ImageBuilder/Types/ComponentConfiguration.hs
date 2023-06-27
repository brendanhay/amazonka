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
-- Module      : Amazonka.ImageBuilder.Types.ComponentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.ComponentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.ComponentParameter
import qualified Amazonka.Prelude as Prelude

-- | Configuration details of the component.
--
-- /See:/ 'newComponentConfiguration' smart constructor.
data ComponentConfiguration = ComponentConfiguration'
  { -- | A group of parameter settings that Image Builder uses to configure the
    -- component for a specific recipe.
    parameters :: Prelude.Maybe (Prelude.NonEmpty ComponentParameter),
    -- | The Amazon Resource Name (ARN) of the component.
    componentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'componentConfiguration_parameters' - A group of parameter settings that Image Builder uses to configure the
-- component for a specific recipe.
--
-- 'componentArn', 'componentConfiguration_componentArn' - The Amazon Resource Name (ARN) of the component.
newComponentConfiguration ::
  -- | 'componentArn'
  Prelude.Text ->
  ComponentConfiguration
newComponentConfiguration pComponentArn_ =
  ComponentConfiguration'
    { parameters =
        Prelude.Nothing,
      componentArn = pComponentArn_
    }

-- | A group of parameter settings that Image Builder uses to configure the
-- component for a specific recipe.
componentConfiguration_parameters :: Lens.Lens' ComponentConfiguration (Prelude.Maybe (Prelude.NonEmpty ComponentParameter))
componentConfiguration_parameters = Lens.lens (\ComponentConfiguration' {parameters} -> parameters) (\s@ComponentConfiguration' {} a -> s {parameters = a} :: ComponentConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the component.
componentConfiguration_componentArn :: Lens.Lens' ComponentConfiguration Prelude.Text
componentConfiguration_componentArn = Lens.lens (\ComponentConfiguration' {componentArn} -> componentArn) (\s@ComponentConfiguration' {} a -> s {componentArn = a} :: ComponentConfiguration)

instance Data.FromJSON ComponentConfiguration where
  parseJSON =
    Data.withObject
      "ComponentConfiguration"
      ( \x ->
          ComponentConfiguration'
            Prelude.<$> (x Data..:? "parameters")
            Prelude.<*> (x Data..: "componentArn")
      )

instance Prelude.Hashable ComponentConfiguration where
  hashWithSalt _salt ComponentConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` componentArn

instance Prelude.NFData ComponentConfiguration where
  rnf ComponentConfiguration' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf componentArn

instance Data.ToJSON ComponentConfiguration where
  toJSON ComponentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("componentArn" Data..= componentArn)
          ]
      )
