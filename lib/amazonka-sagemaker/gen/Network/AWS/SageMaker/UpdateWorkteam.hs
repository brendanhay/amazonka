{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing work team with new member definitions or description.
module Network.AWS.SageMaker.UpdateWorkteam
  ( -- * Creating a request
    UpdateWorkteam (..),
    mkUpdateWorkteam,

    -- ** Request lenses
    uwNotificationConfiguration,
    uwMemberDefinitions,
    uwDescription,
    uwWorkteamName,

    -- * Destructuring the response
    UpdateWorkteamResponse (..),
    mkUpdateWorkteamResponse,

    -- ** Response lenses
    uwrsResponseStatus,
    uwrsWorkteam,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateWorkteam' smart constructor.
data UpdateWorkteam = UpdateWorkteam'
  { notificationConfiguration ::
      Lude.Maybe NotificationConfiguration,
    memberDefinitions ::
      Lude.Maybe (Lude.NonEmpty MemberDefinition),
    description :: Lude.Maybe Lude.Text,
    workteamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkteam' with the minimum fields required to make a request.
--
-- * 'description' - An updated description for the work team.
-- * 'memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . You should not provide input for both of these parameters in a single request.
-- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
-- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ . Be aware that user groups that are already in the work team must also be listed in @Groups@ when you make this request to remain on the work team. If you do not include these user groups, they will no longer be associated with the work team you update.
-- * 'notificationConfiguration' - Configures SNS topic notifications for available or expiring work items
-- * 'workteamName' - The name of the work team to update.
mkUpdateWorkteam ::
  -- | 'workteamName'
  Lude.Text ->
  UpdateWorkteam
mkUpdateWorkteam pWorkteamName_ =
  UpdateWorkteam'
    { notificationConfiguration = Lude.Nothing,
      memberDefinitions = Lude.Nothing,
      description = Lude.Nothing,
      workteamName = pWorkteamName_
    }

-- | Configures SNS topic notifications for available or expiring work items
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwNotificationConfiguration :: Lens.Lens' UpdateWorkteam (Lude.Maybe NotificationConfiguration)
uwNotificationConfiguration = Lens.lens (notificationConfiguration :: UpdateWorkteam -> Lude.Maybe NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: UpdateWorkteam)
{-# DEPRECATED uwNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . You should not provide input for both of these parameters in a single request.
-- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
-- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ . Be aware that user groups that are already in the work team must also be listed in @Groups@ when you make this request to remain on the work team. If you do not include these user groups, they will no longer be associated with the work team you update.
--
-- /Note:/ Consider using 'memberDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwMemberDefinitions :: Lens.Lens' UpdateWorkteam (Lude.Maybe (Lude.NonEmpty MemberDefinition))
uwMemberDefinitions = Lens.lens (memberDefinitions :: UpdateWorkteam -> Lude.Maybe (Lude.NonEmpty MemberDefinition)) (\s a -> s {memberDefinitions = a} :: UpdateWorkteam)
{-# DEPRECATED uwMemberDefinitions "Use generic-lens or generic-optics with 'memberDefinitions' instead." #-}

-- | An updated description for the work team.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwDescription :: Lens.Lens' UpdateWorkteam (Lude.Maybe Lude.Text)
uwDescription = Lens.lens (description :: UpdateWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateWorkteam)
{-# DEPRECATED uwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the work team to update.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwWorkteamName :: Lens.Lens' UpdateWorkteam Lude.Text
uwWorkteamName = Lens.lens (workteamName :: UpdateWorkteam -> Lude.Text) (\s a -> s {workteamName = a} :: UpdateWorkteam)
{-# DEPRECATED uwWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

instance Lude.AWSRequest UpdateWorkteam where
  type Rs UpdateWorkteam = UpdateWorkteamResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateWorkteamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Workteam")
      )

instance Lude.ToHeaders UpdateWorkteam where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateWorkteam" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWorkteam where
  toJSON UpdateWorkteam' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationConfiguration" Lude..=)
              Lude.<$> notificationConfiguration,
            ("MemberDefinitions" Lude..=) Lude.<$> memberDefinitions,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("WorkteamName" Lude..= workteamName)
          ]
      )

instance Lude.ToPath UpdateWorkteam where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWorkteam where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWorkteamResponse' smart constructor.
data UpdateWorkteamResponse = UpdateWorkteamResponse'
  { responseStatus ::
      Lude.Int,
    workteam :: Workteam
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkteamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'workteam' - A @Workteam@ object that describes the updated work team.
mkUpdateWorkteamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'workteam'
  Workteam ->
  UpdateWorkteamResponse
mkUpdateWorkteamResponse pResponseStatus_ pWorkteam_ =
  UpdateWorkteamResponse'
    { responseStatus = pResponseStatus_,
      workteam = pWorkteam_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrsResponseStatus :: Lens.Lens' UpdateWorkteamResponse Lude.Int
uwrsResponseStatus = Lens.lens (responseStatus :: UpdateWorkteamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWorkteamResponse)
{-# DEPRECATED uwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A @Workteam@ object that describes the updated work team.
--
-- /Note:/ Consider using 'workteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwrsWorkteam :: Lens.Lens' UpdateWorkteamResponse Workteam
uwrsWorkteam = Lens.lens (workteam :: UpdateWorkteamResponse -> Workteam) (\s a -> s {workteam = a} :: UpdateWorkteamResponse)
{-# DEPRECATED uwrsWorkteam "Use generic-lens or generic-optics with 'workteam' instead." #-}
