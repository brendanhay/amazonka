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
-- Module      : Amazonka.Personalize.Types.AutoMLResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.AutoMLResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When the solution performs AutoML (@performAutoML@ is true in
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>),
-- specifies the recipe that best optimized the specified metric.
--
-- /See:/ 'newAutoMLResult' smart constructor.
data AutoMLResult = AutoMLResult'
  { -- | The Amazon Resource Name (ARN) of the best recipe.
    bestRecipeArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bestRecipeArn', 'autoMLResult_bestRecipeArn' - The Amazon Resource Name (ARN) of the best recipe.
newAutoMLResult ::
  AutoMLResult
newAutoMLResult =
  AutoMLResult' {bestRecipeArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the best recipe.
autoMLResult_bestRecipeArn :: Lens.Lens' AutoMLResult (Prelude.Maybe Prelude.Text)
autoMLResult_bestRecipeArn = Lens.lens (\AutoMLResult' {bestRecipeArn} -> bestRecipeArn) (\s@AutoMLResult' {} a -> s {bestRecipeArn = a} :: AutoMLResult)

instance Data.FromJSON AutoMLResult where
  parseJSON =
    Data.withObject
      "AutoMLResult"
      ( \x ->
          AutoMLResult'
            Prelude.<$> (x Data..:? "bestRecipeArn")
      )

instance Prelude.Hashable AutoMLResult where
  hashWithSalt _salt AutoMLResult' {..} =
    _salt `Prelude.hashWithSalt` bestRecipeArn

instance Prelude.NFData AutoMLResult where
  rnf AutoMLResult' {..} = Prelude.rnf bestRecipeArn
