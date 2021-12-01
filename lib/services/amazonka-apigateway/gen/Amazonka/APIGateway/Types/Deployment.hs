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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Deployment where

import Amazonka.APIGateway.Types.MethodSnapshot
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An immutable representation of a RestApi resource that can be called by
-- users using Stages. A deployment must be associated with a Stage for it
-- to be callable over the Internet.
--
-- To create a deployment, call @POST@ on the Deployments resource of a
-- RestApi. To view, update, or delete a deployment, call @GET@, @PATCH@,
-- or @DELETE@ on the specified deployment resource
-- (@\/restapis\/{restapi_id}\/deployments\/{deployment_id}@).
--
-- RestApi, Deployments, Stage,
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI>,
-- <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | A summary of the RestApi at the date and time that the deployment
    -- resource was created.
    apiSummary :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text MethodSnapshot)),
    -- | The date and time that the deployment resource was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The identifier for the deployment resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description for the deployment resource.
    description :: Prelude.Maybe Prelude.Text
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
-- 'apiSummary', 'deployment_apiSummary' - A summary of the RestApi at the date and time that the deployment
-- resource was created.
--
-- 'createdDate', 'deployment_createdDate' - The date and time that the deployment resource was created.
--
-- 'id', 'deployment_id' - The identifier for the deployment resource.
--
-- 'description', 'deployment_description' - The description for the deployment resource.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { apiSummary = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A summary of the RestApi at the date and time that the deployment
-- resource was created.
deployment_apiSummary :: Lens.Lens' Deployment (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text MethodSnapshot)))
deployment_apiSummary = Lens.lens (\Deployment' {apiSummary} -> apiSummary) (\s@Deployment' {} a -> s {apiSummary = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the deployment resource was created.
deployment_createdDate :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_createdDate = Lens.lens (\Deployment' {createdDate} -> createdDate) (\s@Deployment' {} a -> s {createdDate = a} :: Deployment) Prelude.. Lens.mapping Core._Time

-- | The identifier for the deployment resource.
deployment_id :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_id = Lens.lens (\Deployment' {id} -> id) (\s@Deployment' {} a -> s {id = a} :: Deployment)

-- | The description for the deployment resource.
deployment_description :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Core..:? "apiSummary" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt salt' Deployment' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` apiSummary

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf apiSummary
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdDate
