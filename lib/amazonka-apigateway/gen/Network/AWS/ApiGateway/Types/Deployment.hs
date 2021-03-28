{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.Deployment
  ( Deployment (..)
  -- * Smart constructor
  , mkDeployment
  -- * Lenses
  , dApiSummary
  , dCreatedDate
  , dDescription
  , dId
  ) where

import qualified Network.AWS.ApiGateway.Types.MethodSnapshot as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An immutable representation of a 'RestApi' resource that can be called by users using 'Stages' . A deployment must be associated with a 'Stage' for it to be callable over the Internet.
--
-- To create a deployment, call @POST@ on the 'Deployments' resource of a 'RestApi' . To view, update, or delete a deployment, call @GET@ , @PATCH@ , or @DELETE@ on the specified deployment resource (@/restapis/{restapi_id}/deployments/{deployment_id}@ ).'RestApi' , 'Deployments' , 'Stage' , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs> 
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { apiSummary :: Core.Maybe (Core.HashMap Core.Text (Core.HashMap Core.Text Types.MethodSnapshot))
    -- ^ A summary of the 'RestApi' at the date and time that the deployment resource was created.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the deployment resource was created.
  , description :: Core.Maybe Core.Text
    -- ^ The description for the deployment resource.
  , id :: Core.Maybe Core.Text
    -- ^ The identifier for the deployment resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Deployment' value with any optional fields omitted.
mkDeployment
    :: Deployment
mkDeployment
  = Deployment'{apiSummary = Core.Nothing,
                createdDate = Core.Nothing, description = Core.Nothing,
                id = Core.Nothing}

-- | A summary of the 'RestApi' at the date and time that the deployment resource was created.
--
-- /Note:/ Consider using 'apiSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApiSummary :: Lens.Lens' Deployment (Core.Maybe (Core.HashMap Core.Text (Core.HashMap Core.Text Types.MethodSnapshot)))
dApiSummary = Lens.field @"apiSummary"
{-# INLINEABLE dApiSummary #-}
{-# DEPRECATED apiSummary "Use generic-lens or generic-optics with 'apiSummary' instead"  #-}

-- | The date and time that the deployment resource was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedDate :: Lens.Lens' Deployment (Core.Maybe Core.NominalDiffTime)
dCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE dCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The description for the deployment resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dDescription = Lens.field @"description"
{-# INLINEABLE dDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier for the deployment resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Deployment (Core.Maybe Core.Text)
dId = Lens.field @"id"
{-# INLINEABLE dId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON Deployment where
        parseJSON
          = Core.withObject "Deployment" Core.$
              \ x ->
                Deployment' Core.<$>
                  (x Core..:? "apiSummary") Core.<*> x Core..:? "createdDate"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "id"
