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
-- Module      : Network.AWS.APIGateway.Types.Deployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Deployment where

import Network.AWS.APIGateway.Types.MethodSnapshot
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
  { -- | The date and time that the deployment resource was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The identifier for the deployment resource.
    id :: Core.Maybe Core.Text,
    -- | A summary of the RestApi at the date and time that the deployment
    -- resource was created.
    apiSummary :: Core.Maybe (Core.HashMap Core.Text (Core.HashMap Core.Text MethodSnapshot)),
    -- | The description for the deployment resource.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'deployment_createdDate' - The date and time that the deployment resource was created.
--
-- 'id', 'deployment_id' - The identifier for the deployment resource.
--
-- 'apiSummary', 'deployment_apiSummary' - A summary of the RestApi at the date and time that the deployment
-- resource was created.
--
-- 'description', 'deployment_description' - The description for the deployment resource.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { createdDate = Core.Nothing,
      id = Core.Nothing,
      apiSummary = Core.Nothing,
      description = Core.Nothing
    }

-- | The date and time that the deployment resource was created.
deployment_createdDate :: Lens.Lens' Deployment (Core.Maybe Core.UTCTime)
deployment_createdDate = Lens.lens (\Deployment' {createdDate} -> createdDate) (\s@Deployment' {} a -> s {createdDate = a} :: Deployment) Core.. Lens.mapping Core._Time

-- | The identifier for the deployment resource.
deployment_id :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_id = Lens.lens (\Deployment' {id} -> id) (\s@Deployment' {} a -> s {id = a} :: Deployment)

-- | A summary of the RestApi at the date and time that the deployment
-- resource was created.
deployment_apiSummary :: Lens.Lens' Deployment (Core.Maybe (Core.HashMap Core.Text (Core.HashMap Core.Text MethodSnapshot)))
deployment_apiSummary = Lens.lens (\Deployment' {apiSummary} -> apiSummary) (\s@Deployment' {} a -> s {apiSummary = a} :: Deployment) Core.. Lens.mapping Lens._Coerce

-- | The description for the deployment resource.
deployment_description :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_description = Lens.lens (\Deployment' {description} -> description) (\s@Deployment' {} a -> s {description = a} :: Deployment)

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "apiSummary" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable Deployment

instance Core.NFData Deployment
