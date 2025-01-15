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
-- Module      : Amazonka.Personalize.Types.AutoMLConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.AutoMLConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When the solution performs AutoML (@performAutoML@ is true in
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>),
-- Amazon Personalize determines which recipe, from the specified list,
-- optimizes the given metric. Amazon Personalize then uses that recipe for
-- the solution.
--
-- /See:/ 'newAutoMLConfig' smart constructor.
data AutoMLConfig = AutoMLConfig'
  { -- | The metric to optimize.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The list of candidate recipes.
    recipeList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'autoMLConfig_metricName' - The metric to optimize.
--
-- 'recipeList', 'autoMLConfig_recipeList' - The list of candidate recipes.
newAutoMLConfig ::
  AutoMLConfig
newAutoMLConfig =
  AutoMLConfig'
    { metricName = Prelude.Nothing,
      recipeList = Prelude.Nothing
    }

-- | The metric to optimize.
autoMLConfig_metricName :: Lens.Lens' AutoMLConfig (Prelude.Maybe Prelude.Text)
autoMLConfig_metricName = Lens.lens (\AutoMLConfig' {metricName} -> metricName) (\s@AutoMLConfig' {} a -> s {metricName = a} :: AutoMLConfig)

-- | The list of candidate recipes.
autoMLConfig_recipeList :: Lens.Lens' AutoMLConfig (Prelude.Maybe [Prelude.Text])
autoMLConfig_recipeList = Lens.lens (\AutoMLConfig' {recipeList} -> recipeList) (\s@AutoMLConfig' {} a -> s {recipeList = a} :: AutoMLConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutoMLConfig where
  parseJSON =
    Data.withObject
      "AutoMLConfig"
      ( \x ->
          AutoMLConfig'
            Prelude.<$> (x Data..:? "metricName")
            Prelude.<*> (x Data..:? "recipeList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AutoMLConfig where
  hashWithSalt _salt AutoMLConfig' {..} =
    _salt
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` recipeList

instance Prelude.NFData AutoMLConfig where
  rnf AutoMLConfig' {..} =
    Prelude.rnf metricName `Prelude.seq`
      Prelude.rnf recipeList

instance Data.ToJSON AutoMLConfig where
  toJSON AutoMLConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metricName" Data..=) Prelude.<$> metricName,
            ("recipeList" Data..=) Prelude.<$> recipeList
          ]
      )
