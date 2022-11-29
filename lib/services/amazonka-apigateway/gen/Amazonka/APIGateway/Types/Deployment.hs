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
-- Module      : Amazonka.APIGateway.Types.Deployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Deployment where

import Amazonka.APIGateway.Types.MethodSnapshot
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An immutable representation of a RestApi resource that can be called by
-- users using Stages. A deployment must be associated with a Stage for it
-- to be callable over the Internet.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The description for the deployment resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the deployment resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the deployment resource was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | A summary of the RestApi at the date and time that the deployment
    -- resource was created.
    apiSummary :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text MethodSnapshot))
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'deployment_description' - The description for the deployment resource.
--
-- 'id', 'deployment_id' - The identifier for the deployment resource.
--
-- 'createdDate', 'deployment_createdDate' - The date and time that the deployment resource was created.
--
-- 'apiSummary', 'deployment_apiSummary' - A summary of the RestApi at the date and time that the deployment
-- resource was created.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      apiSummary = Prelude.Nothing
    }

-- | The description for the deployment resource.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

-- | The identifier for the deployment resource.
deployment_id :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_id = Lens.lens (\Deployment' {id} -> id) (\s@Deployment' {} a -> s {id = a} :: Deployment)

-- | The date and time that the deployment resource was created.
deployment_createdDate :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_createdDate = Lens.lens (\Deployment' {createdDate} -> createdDate) (\s@Deployment' {} a -> s {createdDate = a} :: Deployment) Prelude.. Lens.mapping Core._Time

-- | A summary of the RestApi at the date and time that the deployment
-- resource was created.
deployment_apiSummary :: Lens.Lens' Deployment (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text MethodSnapshot)))
deployment_apiSummary = Lens.lens (\Deployment' {apiSummary} -> apiSummary) (\s@Deployment' {} a -> s {apiSummary = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "apiSummary" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` apiSummary

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf apiSummary
