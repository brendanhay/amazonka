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
    caIamRole,
    caDefaultInstanceName,
    caDescription,
    caExpirationDate,
    caRegistrationLimit,
    caTags,

    -- * Destructuring the response
    CreateActivationResponse (..),
    mkCreateActivationResponse,

    -- ** Response lenses
    crsActivationCode,
    crsActivationId,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateActivation' smart constructor.
data CreateActivation = CreateActivation'
  { -- | The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance. This IAM role must provide AssumeRole permissions for the Systems Manager service principal @ssm.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment> in the /AWS Systems Manager User Guide/ .
    iamRole :: Types.IamRole,
    -- | The name of the registered, managed instance as it will appear in the Systems Manager console or when you use the AWS command line tools to list Systems Manager resources.
    --
    -- /Important:/ Do not enter personally identifiable information in this field.
    defaultInstanceName :: Core.Maybe Types.DefaultInstanceName,
    -- | A user-defined description of the resource that you want to register with Systems Manager.
    --
    -- /Important:/ Do not enter personally identifiable information in this field.
    description :: Core.Maybe Types.ActivationDescription,
    -- | The date by which this activation request should expire. The default value is 24 hours.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Specify the maximum number of managed instances you want to register. The default value is 1 instance.
    registrationLimit :: Core.Maybe Core.Natural,
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
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateActivation' value with any optional fields omitted.
mkCreateActivation ::
  -- | 'iamRole'
  Types.IamRole ->
  CreateActivation
mkCreateActivation iamRole =
  CreateActivation'
    { iamRole,
      defaultInstanceName = Core.Nothing,
      description = Core.Nothing,
      expirationDate = Core.Nothing,
      registrationLimit = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Amazon Identity and Access Management (IAM) role that you want to assign to the managed instance. This IAM role must provide AssumeRole permissions for the Systems Manager service principal @ssm.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caIamRole :: Lens.Lens' CreateActivation Types.IamRole
caIamRole = Lens.field @"iamRole"
{-# DEPRECATED caIamRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

-- | The name of the registered, managed instance as it will appear in the Systems Manager console or when you use the AWS command line tools to list Systems Manager resources.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'defaultInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDefaultInstanceName :: Lens.Lens' CreateActivation (Core.Maybe Types.DefaultInstanceName)
caDefaultInstanceName = Lens.field @"defaultInstanceName"
{-# DEPRECATED caDefaultInstanceName "Use generic-lens or generic-optics with 'defaultInstanceName' instead." #-}

-- | A user-defined description of the resource that you want to register with Systems Manager.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateActivation (Core.Maybe Types.ActivationDescription)
caDescription = Lens.field @"description"
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date by which this activation request should expire. The default value is 24 hours.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caExpirationDate :: Lens.Lens' CreateActivation (Core.Maybe Core.NominalDiffTime)
caExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED caExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Specify the maximum number of managed instances you want to register. The default value is 1 instance.
--
-- /Note:/ Consider using 'registrationLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRegistrationLimit :: Lens.Lens' CreateActivation (Core.Maybe Core.Natural)
caRegistrationLimit = Lens.field @"registrationLimit"
{-# DEPRECATED caRegistrationLimit "Use generic-lens or generic-optics with 'registrationLimit' instead." #-}

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
caTags :: Lens.Lens' CreateActivation (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateActivation where
  toJSON CreateActivation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IamRole" Core..= iamRole),
            ("DefaultInstanceName" Core..=) Core.<$> defaultInstanceName,
            ("Description" Core..=) Core.<$> description,
            ("ExpirationDate" Core..=) Core.<$> expirationDate,
            ("RegistrationLimit" Core..=) Core.<$> registrationLimit,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateActivation where
  type Rs CreateActivation = CreateActivationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.CreateActivation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateActivationResponse'
            Core.<$> (x Core..:? "ActivationCode")
            Core.<*> (x Core..:? "ActivationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateActivationResponse' smart constructor.
data CreateActivationResponse = CreateActivationResponse'
  { -- | The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
    activationCode :: Core.Maybe Types.ActivationCode,
    -- | The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
    activationId :: Core.Maybe Types.ActivationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateActivationResponse' value with any optional fields omitted.
mkCreateActivationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateActivationResponse
mkCreateActivationResponse responseStatus =
  CreateActivationResponse'
    { activationCode = Core.Nothing,
      activationId = Core.Nothing,
      responseStatus
    }

-- | The code the system generates when it processes the activation. The activation code functions like a password to validate the activation ID.
--
-- /Note:/ Consider using 'activationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsActivationCode :: Lens.Lens' CreateActivationResponse (Core.Maybe Types.ActivationCode)
crsActivationCode = Lens.field @"activationCode"
{-# DEPRECATED crsActivationCode "Use generic-lens or generic-optics with 'activationCode' instead." #-}

-- | The ID number generated by the system when it processed the activation. The activation ID functions like a user name.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsActivationId :: Lens.Lens' CreateActivationResponse (Core.Maybe Types.ActivationId)
crsActivationId = Lens.field @"activationId"
{-# DEPRECATED crsActivationId "Use generic-lens or generic-optics with 'activationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateActivationResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
