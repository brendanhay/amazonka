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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | A list of parameter values sent to targets that resolved during the
    -- Automation execution.
    parameterValues :: Prelude.Maybe [Prelude.Text],
    -- | A boolean value indicating whether the resolved target list is
    -- truncated.
    truncated :: Prelude.Maybe Prelude.Bool
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
-- 'parameterValues', 'resolvedTargets_parameterValues' - A list of parameter values sent to targets that resolved during the
-- Automation execution.
--
-- 'truncated', 'resolvedTargets_truncated' - A boolean value indicating whether the resolved target list is
-- truncated.
newResolvedTargets ::
  ResolvedTargets
newResolvedTargets =
  ResolvedTargets'
    { parameterValues = Prelude.Nothing,
      truncated = Prelude.Nothing
    }

-- | A list of parameter values sent to targets that resolved during the
-- Automation execution.
resolvedTargets_parameterValues :: Lens.Lens' ResolvedTargets (Prelude.Maybe [Prelude.Text])
resolvedTargets_parameterValues = Lens.lens (\ResolvedTargets' {parameterValues} -> parameterValues) (\s@ResolvedTargets' {} a -> s {parameterValues = a} :: ResolvedTargets) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value indicating whether the resolved target list is
-- truncated.
resolvedTargets_truncated :: Lens.Lens' ResolvedTargets (Prelude.Maybe Prelude.Bool)
resolvedTargets_truncated = Lens.lens (\ResolvedTargets' {truncated} -> truncated) (\s@ResolvedTargets' {} a -> s {truncated = a} :: ResolvedTargets)

instance Data.FromJSON ResolvedTargets where
  parseJSON =
    Data.withObject
      "ResolvedTargets"
      ( \x ->
          ResolvedTargets'
            Prelude.<$> ( x
                            Data..:? "ParameterValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Truncated")
      )

instance Prelude.Hashable ResolvedTargets where
  hashWithSalt _salt ResolvedTargets' {..} =
    _salt
      `Prelude.hashWithSalt` parameterValues
      `Prelude.hashWithSalt` truncated

instance Prelude.NFData ResolvedTargets where
  rnf ResolvedTargets' {..} =
    Prelude.rnf parameterValues
      `Prelude.seq` Prelude.rnf truncated
