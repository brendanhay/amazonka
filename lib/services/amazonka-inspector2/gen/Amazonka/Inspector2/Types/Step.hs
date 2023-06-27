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
-- Module      : Amazonka.Inspector2.Types.Step
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Step where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the step associated with a finding.
--
-- /See:/ 'newStep' smart constructor.
data Step = Step'
  { -- | The component ID.
    componentId :: Prelude.Text,
    -- | The component type.
    componentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Step' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentId', 'step_componentId' - The component ID.
--
-- 'componentType', 'step_componentType' - The component type.
newStep ::
  -- | 'componentId'
  Prelude.Text ->
  -- | 'componentType'
  Prelude.Text ->
  Step
newStep pComponentId_ pComponentType_ =
  Step'
    { componentId = pComponentId_,
      componentType = pComponentType_
    }

-- | The component ID.
step_componentId :: Lens.Lens' Step Prelude.Text
step_componentId = Lens.lens (\Step' {componentId} -> componentId) (\s@Step' {} a -> s {componentId = a} :: Step)

-- | The component type.
step_componentType :: Lens.Lens' Step Prelude.Text
step_componentType = Lens.lens (\Step' {componentType} -> componentType) (\s@Step' {} a -> s {componentType = a} :: Step)

instance Data.FromJSON Step where
  parseJSON =
    Data.withObject
      "Step"
      ( \x ->
          Step'
            Prelude.<$> (x Data..: "componentId")
            Prelude.<*> (x Data..: "componentType")
      )

instance Prelude.Hashable Step where
  hashWithSalt _salt Step' {..} =
    _salt
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` componentType

instance Prelude.NFData Step where
  rnf Step' {..} =
    Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf componentType
