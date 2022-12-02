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
-- Module      : Amazonka.SSM.Types.ResolvedTargets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResolvedTargets where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about targets that resolved during the Automation execution.
--
-- /See:/ 'newResolvedTargets' smart constructor.
data ResolvedTargets = ResolvedTargets'
  { -- | A boolean value indicating whether the resolved target list is
    -- truncated.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | A list of parameter values sent to targets that resolved during the
    -- Automation execution.
    parameterValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolvedTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truncated', 'resolvedTargets_truncated' - A boolean value indicating whether the resolved target list is
-- truncated.
--
-- 'parameterValues', 'resolvedTargets_parameterValues' - A list of parameter values sent to targets that resolved during the
-- Automation execution.
newResolvedTargets ::
  ResolvedTargets
newResolvedTargets =
  ResolvedTargets'
    { truncated = Prelude.Nothing,
      parameterValues = Prelude.Nothing
    }

-- | A boolean value indicating whether the resolved target list is
-- truncated.
resolvedTargets_truncated :: Lens.Lens' ResolvedTargets (Prelude.Maybe Prelude.Bool)
resolvedTargets_truncated = Lens.lens (\ResolvedTargets' {truncated} -> truncated) (\s@ResolvedTargets' {} a -> s {truncated = a} :: ResolvedTargets)

-- | A list of parameter values sent to targets that resolved during the
-- Automation execution.
resolvedTargets_parameterValues :: Lens.Lens' ResolvedTargets (Prelude.Maybe [Prelude.Text])
resolvedTargets_parameterValues = Lens.lens (\ResolvedTargets' {parameterValues} -> parameterValues) (\s@ResolvedTargets' {} a -> s {parameterValues = a} :: ResolvedTargets) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResolvedTargets where
  parseJSON =
    Data.withObject
      "ResolvedTargets"
      ( \x ->
          ResolvedTargets'
            Prelude.<$> (x Data..:? "Truncated")
            Prelude.<*> ( x Data..:? "ParameterValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ResolvedTargets where
  hashWithSalt _salt ResolvedTargets' {..} =
    _salt `Prelude.hashWithSalt` truncated
      `Prelude.hashWithSalt` parameterValues

instance Prelude.NFData ResolvedTargets where
  rnf ResolvedTargets' {..} =
    Prelude.rnf truncated
      `Prelude.seq` Prelude.rnf parameterValues
