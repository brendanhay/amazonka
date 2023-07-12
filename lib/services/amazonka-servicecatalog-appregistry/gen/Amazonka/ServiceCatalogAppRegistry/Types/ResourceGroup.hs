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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroupState

-- | The information about the resource group integration.
--
-- /See:/ 'newResourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
  { -- | The Amazon resource name (ARN) of the resource group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The error message that generates when the propagation process for the
    -- resource group fails.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The state of the propagation process for the resource group. The states
    -- includes:
    --
    -- @CREATING @if the resource group is in the process of being created.
    --
    -- @CREATE_COMPLETE@ if the resource group was created successfully.
    --
    -- @CREATE_FAILED@ if the resource group failed to be created.
    --
    -- @UPDATING@ if the resource group is in the process of being updated.
    --
    -- @UPDATE_COMPLETE@ if the resource group updated successfully.
    --
    -- @UPDATE_FAILED@ if the resource group could not update successfully.
    state :: Prelude.Maybe ResourceGroupState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceGroup_arn' - The Amazon resource name (ARN) of the resource group.
--
-- 'errorMessage', 'resourceGroup_errorMessage' - The error message that generates when the propagation process for the
-- resource group fails.
--
-- 'state', 'resourceGroup_state' - The state of the propagation process for the resource group. The states
-- includes:
--
-- @CREATING @if the resource group is in the process of being created.
--
-- @CREATE_COMPLETE@ if the resource group was created successfully.
--
-- @CREATE_FAILED@ if the resource group failed to be created.
--
-- @UPDATING@ if the resource group is in the process of being updated.
--
-- @UPDATE_COMPLETE@ if the resource group updated successfully.
--
-- @UPDATE_FAILED@ if the resource group could not update successfully.
newResourceGroup ::
  ResourceGroup
newResourceGroup =
  ResourceGroup'
    { arn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) of the resource group.
resourceGroup_arn :: Lens.Lens' ResourceGroup (Prelude.Maybe Prelude.Text)
resourceGroup_arn = Lens.lens (\ResourceGroup' {arn} -> arn) (\s@ResourceGroup' {} a -> s {arn = a} :: ResourceGroup)

-- | The error message that generates when the propagation process for the
-- resource group fails.
resourceGroup_errorMessage :: Lens.Lens' ResourceGroup (Prelude.Maybe Prelude.Text)
resourceGroup_errorMessage = Lens.lens (\ResourceGroup' {errorMessage} -> errorMessage) (\s@ResourceGroup' {} a -> s {errorMessage = a} :: ResourceGroup)

-- | The state of the propagation process for the resource group. The states
-- includes:
--
-- @CREATING @if the resource group is in the process of being created.
--
-- @CREATE_COMPLETE@ if the resource group was created successfully.
--
-- @CREATE_FAILED@ if the resource group failed to be created.
--
-- @UPDATING@ if the resource group is in the process of being updated.
--
-- @UPDATE_COMPLETE@ if the resource group updated successfully.
--
-- @UPDATE_FAILED@ if the resource group could not update successfully.
resourceGroup_state :: Lens.Lens' ResourceGroup (Prelude.Maybe ResourceGroupState)
resourceGroup_state = Lens.lens (\ResourceGroup' {state} -> state) (\s@ResourceGroup' {} a -> s {state = a} :: ResourceGroup)

instance Data.FromJSON ResourceGroup where
  parseJSON =
    Data.withObject
      "ResourceGroup"
      ( \x ->
          ResourceGroup'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable ResourceGroup where
  hashWithSalt _salt ResourceGroup' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` state

instance Prelude.NFData ResourceGroup where
  rnf ResourceGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf state
