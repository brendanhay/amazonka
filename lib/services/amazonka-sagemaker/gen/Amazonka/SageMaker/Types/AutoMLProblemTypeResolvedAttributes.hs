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
-- Module      : Amazonka.SageMaker.Types.AutoMLProblemTypeResolvedAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLProblemTypeResolvedAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TabularResolvedAttributes

-- | The resolved attributes specific to the problem type of an AutoML job
-- V2.
--
-- /See:/ 'newAutoMLProblemTypeResolvedAttributes' smart constructor.
data AutoMLProblemTypeResolvedAttributes = AutoMLProblemTypeResolvedAttributes'
  { -- | Defines the resolved attributes for the @TABULAR@ problem type.
    tabularResolvedAttributes :: Prelude.Maybe TabularResolvedAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLProblemTypeResolvedAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tabularResolvedAttributes', 'autoMLProblemTypeResolvedAttributes_tabularResolvedAttributes' - Defines the resolved attributes for the @TABULAR@ problem type.
newAutoMLProblemTypeResolvedAttributes ::
  AutoMLProblemTypeResolvedAttributes
newAutoMLProblemTypeResolvedAttributes =
  AutoMLProblemTypeResolvedAttributes'
    { tabularResolvedAttributes =
        Prelude.Nothing
    }

-- | Defines the resolved attributes for the @TABULAR@ problem type.
autoMLProblemTypeResolvedAttributes_tabularResolvedAttributes :: Lens.Lens' AutoMLProblemTypeResolvedAttributes (Prelude.Maybe TabularResolvedAttributes)
autoMLProblemTypeResolvedAttributes_tabularResolvedAttributes = Lens.lens (\AutoMLProblemTypeResolvedAttributes' {tabularResolvedAttributes} -> tabularResolvedAttributes) (\s@AutoMLProblemTypeResolvedAttributes' {} a -> s {tabularResolvedAttributes = a} :: AutoMLProblemTypeResolvedAttributes)

instance
  Data.FromJSON
    AutoMLProblemTypeResolvedAttributes
  where
  parseJSON =
    Data.withObject
      "AutoMLProblemTypeResolvedAttributes"
      ( \x ->
          AutoMLProblemTypeResolvedAttributes'
            Prelude.<$> (x Data..:? "TabularResolvedAttributes")
      )

instance
  Prelude.Hashable
    AutoMLProblemTypeResolvedAttributes
  where
  hashWithSalt
    _salt
    AutoMLProblemTypeResolvedAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` tabularResolvedAttributes

instance
  Prelude.NFData
    AutoMLProblemTypeResolvedAttributes
  where
  rnf AutoMLProblemTypeResolvedAttributes' {..} =
    Prelude.rnf tabularResolvedAttributes
