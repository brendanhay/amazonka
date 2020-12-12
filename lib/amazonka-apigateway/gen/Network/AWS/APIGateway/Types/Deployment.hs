{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Deployment
  ( Deployment (..),

    -- * Smart constructor
    mkDeployment,

    -- * Lenses
    dApiSummary,
    dCreatedDate,
    dId,
    dDescription,
  )
where

import Network.AWS.APIGateway.Types.MethodSnapshot
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An immutable representation of a 'RestApi' resource that can be called by users using 'Stages' . A deployment must be associated with a 'Stage' for it to be callable over the Internet.
--
-- To create a deployment, call @POST@ on the 'Deployments' resource of a 'RestApi' . To view, update, or delete a deployment, call @GET@ , @PATCH@ , or @DELETE@ on the specified deployment resource (@/restapis/{restapi_id}/deployments/{deployment_id}@ ).'RestApi' , 'Deployments' , 'Stage' , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs>
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { apiSummary ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.HashMap Lude.Text (MethodSnapshot))),
    createdDate :: Lude.Maybe Lude.Timestamp,
    id :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- * 'apiSummary' - A summary of the 'RestApi' at the date and time that the deployment resource was created.
-- * 'createdDate' - The date and time that the deployment resource was created.
-- * 'description' - The description for the deployment resource.
-- * 'id' - The identifier for the deployment resource.
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { apiSummary = Lude.Nothing,
      createdDate = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing
    }

-- | A summary of the 'RestApi' at the date and time that the deployment resource was created.
--
-- /Note:/ Consider using 'apiSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApiSummary :: Lens.Lens' Deployment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.HashMap Lude.Text (MethodSnapshot))))
dApiSummary = Lens.lens (apiSummary :: Deployment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.HashMap Lude.Text (MethodSnapshot)))) (\s a -> s {apiSummary = a} :: Deployment)
{-# DEPRECATED dApiSummary "Use generic-lens or generic-optics with 'apiSummary' instead." #-}

-- | The date and time that the deployment resource was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedDate :: Lens.Lens' Deployment (Lude.Maybe Lude.Timestamp)
dCreatedDate = Lens.lens (createdDate :: Deployment -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: Deployment)
{-# DEPRECATED dCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The identifier for the deployment resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dId = Lens.lens (id :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Deployment)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description for the deployment resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dDescription = Lens.lens (description :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Deployment)
{-# DEPRECATED dDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Deployment where
  parseJSON =
    Lude.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Lude.<$> (x Lude..:? "apiSummary" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "description")
      )
