{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new work team for labeling your data. A work team is defined by one or more Amazon Cognito user pools. You must first create the user pools before you can create a work team.
--
-- You cannot create more than 25 work teams in an account and region.
module Network.AWS.SageMaker.CreateWorkteam
  ( -- * Creating a request
    CreateWorkteam (..),
    mkCreateWorkteam,

    -- ** Request lenses
    cwNotificationConfiguration,
    cwMemberDefinitions,
    cwWorkteamName,
    cwWorkforceName,
    cwDescription,
    cwTags,

    -- * Destructuring the response
    CreateWorkteamResponse (..),
    mkCreateWorkteamResponse,

    -- ** Response lenses
    cwfrsWorkteamARN,
    cwfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateWorkteam' smart constructor.
data CreateWorkteam = CreateWorkteam'
  { -- | Configures notification of workers regarding available or expiring work items.
    notificationConfiguration :: Lude.Maybe NotificationConfiguration,
    -- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
    --
    -- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . Do not provide input for both of these parameters in a single request.
    -- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
    -- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ .
    memberDefinitions :: Lude.NonEmpty MemberDefinition,
    -- | The name of the work team. Use this name to identify the work team.
    workteamName :: Lude.Text,
    -- | The name of the workforce.
    workforceName :: Lude.Maybe Lude.Text,
    -- | A description of the work team.
    description :: Lude.Text,
    -- | An array of key-value pairs.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkteam' with the minimum fields required to make a request.
--
-- * 'notificationConfiguration' - Configures notification of workers regarding available or expiring work items.
-- * 'memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . Do not provide input for both of these parameters in a single request.
-- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
-- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ .
-- * 'workteamName' - The name of the work team. Use this name to identify the work team.
-- * 'workforceName' - The name of the workforce.
-- * 'description' - A description of the work team.
-- * 'tags' - An array of key-value pairs.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
mkCreateWorkteam ::
  -- | 'memberDefinitions'
  Lude.NonEmpty MemberDefinition ->
  -- | 'workteamName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateWorkteam
mkCreateWorkteam pMemberDefinitions_ pWorkteamName_ pDescription_ =
  CreateWorkteam'
    { notificationConfiguration = Lude.Nothing,
      memberDefinitions = pMemberDefinitions_,
      workteamName = pWorkteamName_,
      workforceName = Lude.Nothing,
      description = pDescription_,
      tags = Lude.Nothing
    }

-- | Configures notification of workers regarding available or expiring work items.
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwNotificationConfiguration :: Lens.Lens' CreateWorkteam (Lude.Maybe NotificationConfiguration)
cwNotificationConfiguration = Lens.lens (notificationConfiguration :: CreateWorkteam -> Lude.Maybe NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: CreateWorkteam)
{-# DEPRECATED cwNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | A list of @MemberDefinition@ objects that contains objects that identify the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity Provider (IdP). For private workforces created using Amazon Cognito use @CognitoMemberDefinition@ . For workforces created using your own OIDC identity provider (IdP) use @OidcMemberDefinition@ . Do not provide input for both of these parameters in a single request.
-- For workforces created using Amazon Cognito, private work teams correspond to Amazon Cognito /user groups/ within the user pool used to create a workforce. All of the @CognitoMemberDefinition@ objects that make up the member definition must have the same @ClientId@ and @UserPool@ values. To add a Amazon Cognito user group to an existing worker pool, see < Adding groups to a User Pool> . For more information about user pools, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito User Pools> .
-- For workforces created using your own OIDC IdP, specify the user groups that you want to include in your private work team in @OidcMemberDefinition@ by listing those groups in @Groups@ .
--
-- /Note:/ Consider using 'memberDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwMemberDefinitions :: Lens.Lens' CreateWorkteam (Lude.NonEmpty MemberDefinition)
cwMemberDefinitions = Lens.lens (memberDefinitions :: CreateWorkteam -> Lude.NonEmpty MemberDefinition) (\s a -> s {memberDefinitions = a} :: CreateWorkteam)
{-# DEPRECATED cwMemberDefinitions "Use generic-lens or generic-optics with 'memberDefinitions' instead." #-}

-- | The name of the work team. Use this name to identify the work team.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwWorkteamName :: Lens.Lens' CreateWorkteam Lude.Text
cwWorkteamName = Lens.lens (workteamName :: CreateWorkteam -> Lude.Text) (\s a -> s {workteamName = a} :: CreateWorkteam)
{-# DEPRECATED cwWorkteamName "Use generic-lens or generic-optics with 'workteamName' instead." #-}

-- | The name of the workforce.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwWorkforceName :: Lens.Lens' CreateWorkteam (Lude.Maybe Lude.Text)
cwWorkforceName = Lens.lens (workforceName :: CreateWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {workforceName = a} :: CreateWorkteam)
{-# DEPRECATED cwWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

-- | A description of the work team.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwDescription :: Lens.Lens' CreateWorkteam Lude.Text
cwDescription = Lens.lens (description :: CreateWorkteam -> Lude.Text) (\s a -> s {description = a} :: CreateWorkteam)
{-# DEPRECATED cwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An array of key-value pairs.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-resource-tags.html Resource Tag> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-what Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwTags :: Lens.Lens' CreateWorkteam (Lude.Maybe [Tag])
cwTags = Lens.lens (tags :: CreateWorkteam -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateWorkteam)
{-# DEPRECATED cwTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateWorkteam where
  type Rs CreateWorkteam = CreateWorkteamResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWorkteamResponse'
            Lude.<$> (x Lude..?> "WorkteamArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWorkteam where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateWorkteam" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWorkteam where
  toJSON CreateWorkteam' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationConfiguration" Lude..=)
              Lude.<$> notificationConfiguration,
            Lude.Just ("MemberDefinitions" Lude..= memberDefinitions),
            Lude.Just ("WorkteamName" Lude..= workteamName),
            ("WorkforceName" Lude..=) Lude.<$> workforceName,
            Lude.Just ("Description" Lude..= description),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateWorkteam where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWorkteam where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWorkteamResponse' smart constructor.
data CreateWorkteamResponse = CreateWorkteamResponse'
  { -- | The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
    workteamARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateWorkteamResponse' with the minimum fields required to make a request.
--
-- * 'workteamARN' - The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
-- * 'responseStatus' - The response status code.
mkCreateWorkteamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWorkteamResponse
mkCreateWorkteamResponse pResponseStatus_ =
  CreateWorkteamResponse'
    { workteamARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the work team. You can use this ARN to identify the work team.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfrsWorkteamARN :: Lens.Lens' CreateWorkteamResponse (Lude.Maybe Lude.Text)
cwfrsWorkteamARN = Lens.lens (workteamARN :: CreateWorkteamResponse -> Lude.Maybe Lude.Text) (\s a -> s {workteamARN = a} :: CreateWorkteamResponse)
{-# DEPRECATED cwfrsWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwfrsResponseStatus :: Lens.Lens' CreateWorkteamResponse Lude.Int
cwfrsResponseStatus = Lens.lens (responseStatus :: CreateWorkteamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWorkteamResponse)
{-# DEPRECATED cwfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
