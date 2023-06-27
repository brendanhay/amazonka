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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentState
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
import Amazonka.MigrationHubReFactorSpaces.Types.NetworkFabricType
import qualified Amazonka.Prelude as Prelude

-- | The summary information for environments as a response to
-- @ListEnvironments@.
--
-- /See:/ 'newEnvironmentSummary' smart constructor.
data EnvironmentSummary = EnvironmentSummary'
  { -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the environment is created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | A description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Any error associated with the environment resource.
    error :: Prelude.Maybe ErrorResponse,
    -- | A timestamp that indicates when the environment was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The network fabric type of the environment.
    networkFabricType :: Prelude.Maybe NetworkFabricType,
    -- | The Amazon Web Services account ID of the environment owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the environment.
    state :: Prelude.Maybe EnvironmentState,
    -- | The tags assigned to the environment.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The ID of the Transit Gateway set up by the environment.
    transitGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'environmentSummary_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'createdTime', 'environmentSummary_createdTime' - A timestamp that indicates when the environment is created.
--
-- 'description', 'environmentSummary_description' - A description of the environment.
--
-- 'environmentId', 'environmentSummary_environmentId' - The unique identifier of the environment.
--
-- 'error', 'environmentSummary_error' - Any error associated with the environment resource.
--
-- 'lastUpdatedTime', 'environmentSummary_lastUpdatedTime' - A timestamp that indicates when the environment was last updated.
--
-- 'name', 'environmentSummary_name' - The name of the environment.
--
-- 'networkFabricType', 'environmentSummary_networkFabricType' - The network fabric type of the environment.
--
-- 'ownerAccountId', 'environmentSummary_ownerAccountId' - The Amazon Web Services account ID of the environment owner.
--
-- 'state', 'environmentSummary_state' - The current state of the environment.
--
-- 'tags', 'environmentSummary_tags' - The tags assigned to the environment.
--
-- 'transitGatewayId', 'environmentSummary_transitGatewayId' - The ID of the Transit Gateway set up by the environment.
newEnvironmentSummary ::
  EnvironmentSummary
newEnvironmentSummary =
  EnvironmentSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      error = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      networkFabricType = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the environment.
environmentSummary_arn :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_arn = Lens.lens (\EnvironmentSummary' {arn} -> arn) (\s@EnvironmentSummary' {} a -> s {arn = a} :: EnvironmentSummary)

-- | A timestamp that indicates when the environment is created.
environmentSummary_createdTime :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.UTCTime)
environmentSummary_createdTime = Lens.lens (\EnvironmentSummary' {createdTime} -> createdTime) (\s@EnvironmentSummary' {} a -> s {createdTime = a} :: EnvironmentSummary) Prelude.. Lens.mapping Data._Time

-- | A description of the environment.
environmentSummary_description :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_description = Lens.lens (\EnvironmentSummary' {description} -> description) (\s@EnvironmentSummary' {} a -> s {description = a} :: EnvironmentSummary)

-- | The unique identifier of the environment.
environmentSummary_environmentId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_environmentId = Lens.lens (\EnvironmentSummary' {environmentId} -> environmentId) (\s@EnvironmentSummary' {} a -> s {environmentId = a} :: EnvironmentSummary)

-- | Any error associated with the environment resource.
environmentSummary_error :: Lens.Lens' EnvironmentSummary (Prelude.Maybe ErrorResponse)
environmentSummary_error = Lens.lens (\EnvironmentSummary' {error} -> error) (\s@EnvironmentSummary' {} a -> s {error = a} :: EnvironmentSummary)

-- | A timestamp that indicates when the environment was last updated.
environmentSummary_lastUpdatedTime :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.UTCTime)
environmentSummary_lastUpdatedTime = Lens.lens (\EnvironmentSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@EnvironmentSummary' {} a -> s {lastUpdatedTime = a} :: EnvironmentSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the environment.
environmentSummary_name :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_name = Lens.lens (\EnvironmentSummary' {name} -> name) (\s@EnvironmentSummary' {} a -> s {name = a} :: EnvironmentSummary)

-- | The network fabric type of the environment.
environmentSummary_networkFabricType :: Lens.Lens' EnvironmentSummary (Prelude.Maybe NetworkFabricType)
environmentSummary_networkFabricType = Lens.lens (\EnvironmentSummary' {networkFabricType} -> networkFabricType) (\s@EnvironmentSummary' {} a -> s {networkFabricType = a} :: EnvironmentSummary)

-- | The Amazon Web Services account ID of the environment owner.
environmentSummary_ownerAccountId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_ownerAccountId = Lens.lens (\EnvironmentSummary' {ownerAccountId} -> ownerAccountId) (\s@EnvironmentSummary' {} a -> s {ownerAccountId = a} :: EnvironmentSummary)

-- | The current state of the environment.
environmentSummary_state :: Lens.Lens' EnvironmentSummary (Prelude.Maybe EnvironmentState)
environmentSummary_state = Lens.lens (\EnvironmentSummary' {state} -> state) (\s@EnvironmentSummary' {} a -> s {state = a} :: EnvironmentSummary)

-- | The tags assigned to the environment.
environmentSummary_tags :: Lens.Lens' EnvironmentSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
environmentSummary_tags = Lens.lens (\EnvironmentSummary' {tags} -> tags) (\s@EnvironmentSummary' {} a -> s {tags = a} :: EnvironmentSummary) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the Transit Gateway set up by the environment.
environmentSummary_transitGatewayId :: Lens.Lens' EnvironmentSummary (Prelude.Maybe Prelude.Text)
environmentSummary_transitGatewayId = Lens.lens (\EnvironmentSummary' {transitGatewayId} -> transitGatewayId) (\s@EnvironmentSummary' {} a -> s {transitGatewayId = a} :: EnvironmentSummary)

instance Data.FromJSON EnvironmentSummary where
  parseJSON =
    Data.withObject
      "EnvironmentSummary"
      ( \x ->
          EnvironmentSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EnvironmentId")
            Prelude.<*> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NetworkFabricType")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TransitGatewayId")
      )

instance Prelude.Hashable EnvironmentSummary where
  hashWithSalt _salt EnvironmentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkFabricType
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transitGatewayId

instance Prelude.NFData EnvironmentSummary where
  rnf EnvironmentSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkFabricType
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayId
