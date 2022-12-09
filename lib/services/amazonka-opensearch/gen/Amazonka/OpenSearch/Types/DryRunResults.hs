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
-- Module      : Amazonka.OpenSearch.Types.DryRunResults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DryRunResults where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Results of a dry run performed in an update domain request.
--
-- /See:/ 'newDryRunResults' smart constructor.
data DryRunResults = DryRunResults'
  { -- | Specifies the way in which OpenSearch Service will apply an update.
    -- Possible values are:
    --
    -- -   __Blue\/Green__ - The update requires a blue\/green deployment.
    --
    -- -   __DynamicUpdate__ - No blue\/green deployment required
    --
    -- -   __Undetermined__ - The domain is in the middle of an update and
    --     can\'t predict the deployment type. Try again after the update is
    --     complete.
    --
    -- -   __None__ - The request doesn\'t include any configuration changes.
    deploymentType :: Prelude.Maybe Prelude.Text,
    -- | A message corresponding to the deployment type.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DryRunResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentType', 'dryRunResults_deploymentType' - Specifies the way in which OpenSearch Service will apply an update.
-- Possible values are:
--
-- -   __Blue\/Green__ - The update requires a blue\/green deployment.
--
-- -   __DynamicUpdate__ - No blue\/green deployment required
--
-- -   __Undetermined__ - The domain is in the middle of an update and
--     can\'t predict the deployment type. Try again after the update is
--     complete.
--
-- -   __None__ - The request doesn\'t include any configuration changes.
--
-- 'message', 'dryRunResults_message' - A message corresponding to the deployment type.
newDryRunResults ::
  DryRunResults
newDryRunResults =
  DryRunResults'
    { deploymentType = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Specifies the way in which OpenSearch Service will apply an update.
-- Possible values are:
--
-- -   __Blue\/Green__ - The update requires a blue\/green deployment.
--
-- -   __DynamicUpdate__ - No blue\/green deployment required
--
-- -   __Undetermined__ - The domain is in the middle of an update and
--     can\'t predict the deployment type. Try again after the update is
--     complete.
--
-- -   __None__ - The request doesn\'t include any configuration changes.
dryRunResults_deploymentType :: Lens.Lens' DryRunResults (Prelude.Maybe Prelude.Text)
dryRunResults_deploymentType = Lens.lens (\DryRunResults' {deploymentType} -> deploymentType) (\s@DryRunResults' {} a -> s {deploymentType = a} :: DryRunResults)

-- | A message corresponding to the deployment type.
dryRunResults_message :: Lens.Lens' DryRunResults (Prelude.Maybe Prelude.Text)
dryRunResults_message = Lens.lens (\DryRunResults' {message} -> message) (\s@DryRunResults' {} a -> s {message = a} :: DryRunResults)

instance Data.FromJSON DryRunResults where
  parseJSON =
    Data.withObject
      "DryRunResults"
      ( \x ->
          DryRunResults'
            Prelude.<$> (x Data..:? "DeploymentType")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable DryRunResults where
  hashWithSalt _salt DryRunResults' {..} =
    _salt `Prelude.hashWithSalt` deploymentType
      `Prelude.hashWithSalt` message

instance Prelude.NFData DryRunResults where
  rnf DryRunResults' {..} =
    Prelude.rnf deploymentType
      `Prelude.seq` Prelude.rnf message
