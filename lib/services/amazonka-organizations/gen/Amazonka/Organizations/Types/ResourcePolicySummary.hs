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
-- Module      : Amazonka.Organizations.Types.ResourcePolicySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.ResourcePolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains resource policy ID and Amazon Resource Name
-- (ARN).
--
-- /See:/ 'newResourcePolicySummary' smart constructor.
data ResourcePolicySummary = ResourcePolicySummary'
  { -- | The Amazon Resource Name (ARN) of the resource policy.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the resource policy.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourcePolicySummary_arn' - The Amazon Resource Name (ARN) of the resource policy.
--
-- 'id', 'resourcePolicySummary_id' - The unique identifier (ID) of the resource policy.
newResourcePolicySummary ::
  ResourcePolicySummary
newResourcePolicySummary =
  ResourcePolicySummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource policy.
resourcePolicySummary_arn :: Lens.Lens' ResourcePolicySummary (Prelude.Maybe Prelude.Text)
resourcePolicySummary_arn = Lens.lens (\ResourcePolicySummary' {arn} -> arn) (\s@ResourcePolicySummary' {} a -> s {arn = a} :: ResourcePolicySummary)

-- | The unique identifier (ID) of the resource policy.
resourcePolicySummary_id :: Lens.Lens' ResourcePolicySummary (Prelude.Maybe Prelude.Text)
resourcePolicySummary_id = Lens.lens (\ResourcePolicySummary' {id} -> id) (\s@ResourcePolicySummary' {} a -> s {id = a} :: ResourcePolicySummary)

instance Data.FromJSON ResourcePolicySummary where
  parseJSON =
    Data.withObject
      "ResourcePolicySummary"
      ( \x ->
          ResourcePolicySummary'
            Prelude.<$> (x Data..:? "Arn") Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable ResourcePolicySummary where
  hashWithSalt _salt ResourcePolicySummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData ResourcePolicySummary where
  rnf ResourcePolicySummary' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
