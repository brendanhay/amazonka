{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateActivation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an activation code and activation ID you can use to register your on-premises server or virtual machine (VM) with Systems Manager. Registering these machines with Systems Manager makes it possible to manage them using Systems Manager capabilities. You use the activation code and ID when installing SSM Agent on machines in your hybrid environment. For more information about requirements for managing on-premises instances and VMs using Systems Manager, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances.html Setting up AWS Systems Manager for hybrid environments> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.CreateActivation
  ( -- * Creating a request
    CreateActivation (..),
    mkCreateActivation,

    -- ** Request lenses
    caDefaultInstanceName,
    caRegistrationLimit,
    caExpirationDate,
    caDescription,
    caTags,
    caIAMRole,

    -- * Destructuring the response
    CreateActivationResponse (..),
    mkCreateActivationResponse,

    -- ** Response lenses
    crsActivationId,
    crsActivationCode,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateActivation' smart constructor.
data CreateActivation = CreateActivation'
  { -- | The name of the registered, managed instance as it will appear in the Systems Manager console or when you use the AWS command line tools to list Systems Manager resources.
    --
    -- /Important:/ Do not enter personally identifiable information in this field.
    defaultInstanceName :: Lude.Maybe Lude.Text,
    -- | Specify the maximum number of managed instances you want to register. The default value is 1 instance.
    registrationLimit :: Lude.Maybe Lude.Natural,
    -- | The date by which this activation request should expire. The default value is 24 hours.
    expirationDate :: Lude.Maybe Lude.Timestamp,
    -- | A user-defined description of the resource that you want to register with Systems Manager.
    --
    -- /Important:/ Do not enter personally identifiable information in this field.
    description :: Lude.Maybe Lude.Text,
    -- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an activation to identify which servers or virtual machines (VMs) in your on-premises environment you intend to activate. In this case, you could specify the following key name/value pairs:
    --
    --
    --     * @Key=OS,Value=Windows@
    --
    --
    --     * @Key=Environment,Value=Production@
    --
    --
    -- /Important:/ When you install SSM Agent on your on-premises servers and VMs, you specify an activation ID and code. When you specify the activation ID and code, tags assigned to the activation are automatically applied to the on-premises servers or VMs.
    -- You can't add tags to or delete tags from an existing activation. You can tag your on-premises servers and VMs after they connect to Systems Manager for the first time and are assigned a managed instance ID. This means they are listed in the AWS Systems Manager console with an ID that is prefixed with "mi-". For information about how to add tags to your managed instances, see 'AddTagsToResource' . For information about how to remove tags from your managed instances, see 'RemoveTagsFromResource' .
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance. This IAM role must provide AssumeRole permissions for the Systems Manager service principal @ssm.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment> in the /AWS Systems Manager User Guide/ .
    iamRole :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateActivation' with the minimum fields required to make a request.
--
-- * 'defaultInstanceName' - The name of the registered, managed instance as it will appear in the Systems Manager console or when you use the AWS command line tools to list Systems Manager resources.
--
-- /Important:/ Do not enter personally identifiable information in this field.
-- * 'registrationLimit' - Specify the maximum number of managed instances you want to register. The default value is 1 instance.
-- * 'expirationDate' - The date by which this activation request should expire. The default value is 24 hours.
-- * 'description' - A user-defined description of the resource that you want to register with Systems Manager.
--
-- /Important:/ Do not enter personally identifiable information in this field.
-- * 'tags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an activation to identify which servers or virtual machines (VMs) in your on-premises environment you intend to activate. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=Environment,Value=Production@
--
--
-- /Important:/ When you install SSM Agent on your on-premises servers and VMs, you specify an activation ID and code. When you specify the activation ID and code, tags assigned to the activation are automatically applied to the on-premises servers or VMs.
-- You can't add tags to or delete tags from an existing activation. You can tag your on-premises servers and VMs after they connect to Systems Manager for the first time and are assigned a managed instance ID. This means they are listed in the AWS Systems Manager console with an ID that is prefixed with "mi-". For information about how to add tags to your managed instances, see 'AddTagsToResource' . For information about how to remove tags from your managed instances, see 'RemoveTagsFromResource' .
-- * 'iamRole' - The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance. This IAM role must provide AssumeRole permissions for the Systems Manager service principal @ssm.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment> in the /AWS Systems Manager User Guide/ .
mkCreateActivation ::
  -- | 'iamRole'
  Lude.Text ->
  CreateActivation
mkCreateActivation pIAMRole_ =
  CreateActivation'
    { defaultInstanceName = Lude.Nothing,
      registrationLimit = Lude.Nothing,
      expirationDate = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      iamRole = pIAMRole_
    }

-- | The name of the registered, managed instance as it will appear in the Systems Manager console or when you use the AWS command line tools to list Systems Manager resources.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'defaultInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDefaultInstanceName :: Lens.Lens' CreateActivation (Lude.Maybe Lude.Text)
caDefaultInstanceName = Lens.lens (defaultInstanceName :: CreateActivation -> Lude.Maybe Lude.Text) (\s a -> s {defaultInstanceName = a} :: CreateActivation)
{-# DEPRECATED caDefaultInstanceName "Use generic-lens or generic-optics with 'defaultInstanceName' instead." #-}

-- | Specify the maximum number of managed instances you want to register. The default value is 1 instance.
--
-- /Note:/ Consider using 'registrationLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRegistrationLimit :: Lens.Lens' CreateActivation (Lude.Maybe Lude.Natural)
caRegistrationLimit = Lens.lens (registrationLimit :: CreateActivation -> Lude.Maybe Lude.Natural) (\s a -> s {registrationLimit = a} :: CreateActivation)
{-# DEPRECATED caRegistrationLimit "Use generic-lens or generic-optics with 'registrationLimit' instead." #-}

-- | The date by which this activation request should expire. The default value is 24 hours.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caExpirationDate :: Lens.Lens' CreateActivation (Lude.Maybe Lude.Timestamp)
caExpirationDate = Lens.lens (expirationDate :: CreateActivation -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: CreateActivation)
{-# DEPRECATED caExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | A user-defined description of the resource that you want to register with Systems Manager.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateActivation (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateActivation -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateActivation)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an activation to identify which servers or virtual machines (VMs) in your on-premises environment you intend to activate. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=Environment,Value=Production@
--
--
-- /Important:/ When you install SSM Agent on your on-premises servers and VMs, you specify an activation ID and code. When you specify the activation ID and code, tags assigned to the activation are automatically applied to the on-premises servers or VMs.
-- You can't add tags to or delete tags from an existing activation. You can tag your on-premises servers and VMs after they connect to Systems Manager for the first time and are assigned a managed instance ID. This means they are listed in the AWS Systems Manager console with an ID that is prefixed with "mi-". For information about how to add tags to your managed instances, see 'AddTagsToResource' . For information about how to remove tags from your managed instances, see 'RemoveTagsFromResource' .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateActivation (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateActivation -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateActivation)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance. This IAM role must provide AssumeRole permissions for the Systems Manager service principal @ssm.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caIAMRole :: Lens.Lens' CreateActivation Lude.Text
caIAMRole = Lens.lens (iamRole :: CreateActivation -> Lude.Text) (\s a -> s {iamRole = a} :: CreateActivation)
{-# DEPRECATED caIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Lude.AWSRequest CreateActivation where
  type Rs CreateActivation = CreateActivationResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateActivationResponse'
            Lude.<$> (x Lude..?> "ActivationId")
            Lude.<*> (x Lude..?> "ActivationCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateActivation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateActivation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateActivation where
  toJSON CreateActivation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultInstanceName" Lude..=) Lude.<$> defaultInstanceName,
            ("RegistrationLimit" Lude..=) Lude.<$> registrationLimit,
            ("ExpirationDate" Lude..=) Lude.<$> expirationDate,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("IamRole" Lude..= iamRole)
          ]
      )

instance Lude.ToPath CreateActivation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateActivation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateActivationResponse' smart constructor.
data CreateActivationResponse = CreateActivationResponse'
  { -- | The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
    activationId :: Lude.Maybe Lude.Text,
    -- | The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
    activationCode :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateActivationResponse' with the minimum fields required to make a request.
--
-- * 'activationId' - The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
-- * 'activationCode' - The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
-- * 'responseStatus' - The response status code.
mkCreateActivationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateActivationResponse
mkCreateActivationResponse pResponseStatus_ =
  CreateActivationResponse'
    { activationId = Lude.Nothing,
      activationCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsActivationId :: Lens.Lens' CreateActivationResponse (Lude.Maybe Lude.Text)
crsActivationId = Lens.lens (activationId :: CreateActivationResponse -> Lude.Maybe Lude.Text) (\s a -> s {activationId = a} :: CreateActivationResponse)
{-# DEPRECATED crsActivationId "Use generic-lens or generic-optics with 'activationId' instead." #-}

-- | The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
--
-- /Note:/ Consider using 'activationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsActivationCode :: Lens.Lens' CreateActivationResponse (Lude.Maybe Lude.Text)
crsActivationCode = Lens.lens (activationCode :: CreateActivationResponse -> Lude.Maybe Lude.Text) (\s a -> s {activationCode = a} :: CreateActivationResponse)
{-# DEPRECATED crsActivationCode "Use generic-lens or generic-optics with 'activationCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateActivationResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateActivationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateActivationResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
