{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Activation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Activation
  ( Activation (..),

    -- * Smart constructor
    mkActivation,

    -- * Lenses
    aActivationId,
    aCreatedDate,
    aDefaultInstanceName,
    aDescription,
    aExpirationDate,
    aExpired,
    aIamRole,
    aRegistrationLimit,
    aRegistrationsCount,
    aTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ActivationId as Types
import qualified Network.AWS.SSM.Types.DefaultInstanceName as Types
import qualified Network.AWS.SSM.Types.Description as Types
import qualified Network.AWS.SSM.Types.IamRole as Types
import qualified Network.AWS.SSM.Types.Tag as Types

-- | An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.
--
-- /See:/ 'mkActivation' smart constructor.
data Activation = Activation'
  { -- | The ID created by Systems Manager when you submitted the activation.
    activationId :: Core.Maybe Types.ActivationId,
    -- | The date the activation was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A name for the managed instance when it is created.
    defaultInstanceName :: Core.Maybe Types.DefaultInstanceName,
    -- | A user defined description of the activation.
    description :: Core.Maybe Types.Description,
    -- | The date when this activation can no longer be used to register managed instances.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | Whether or not the activation is expired.
    expired :: Core.Maybe Core.Bool,
    -- | The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
    iamRole :: Core.Maybe Types.IamRole,
    -- | The maximum number of managed instances that can be registered using this activation.
    registrationLimit :: Core.Maybe Core.Natural,
    -- | The number of managed instances already registered with this activation.
    registrationsCount :: Core.Maybe Core.Natural,
    -- | Tags assigned to the activation.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Activation' value with any optional fields omitted.
mkActivation ::
  Activation
mkActivation =
  Activation'
    { activationId = Core.Nothing,
      createdDate = Core.Nothing,
      defaultInstanceName = Core.Nothing,
      description = Core.Nothing,
      expirationDate = Core.Nothing,
      expired = Core.Nothing,
      iamRole = Core.Nothing,
      registrationLimit = Core.Nothing,
      registrationsCount = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID created by Systems Manager when you submitted the activation.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActivationId :: Lens.Lens' Activation (Core.Maybe Types.ActivationId)
aActivationId = Lens.field @"activationId"
{-# DEPRECATED aActivationId "Use generic-lens or generic-optics with 'activationId' instead." #-}

-- | The date the activation was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedDate :: Lens.Lens' Activation (Core.Maybe Core.NominalDiffTime)
aCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED aCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A name for the managed instance when it is created.
--
-- /Note:/ Consider using 'defaultInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDefaultInstanceName :: Lens.Lens' Activation (Core.Maybe Types.DefaultInstanceName)
aDefaultInstanceName = Lens.field @"defaultInstanceName"
{-# DEPRECATED aDefaultInstanceName "Use generic-lens or generic-optics with 'defaultInstanceName' instead." #-}

-- | A user defined description of the activation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Activation (Core.Maybe Types.Description)
aDescription = Lens.field @"description"
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date when this activation can no longer be used to register managed instances.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aExpirationDate :: Lens.Lens' Activation (Core.Maybe Core.NominalDiffTime)
aExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED aExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | Whether or not the activation is expired.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aExpired :: Lens.Lens' Activation (Core.Maybe Core.Bool)
aExpired = Lens.field @"expired"
{-# DEPRECATED aExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIamRole :: Lens.Lens' Activation (Core.Maybe Types.IamRole)
aIamRole = Lens.field @"iamRole"
{-# DEPRECATED aIamRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

-- | The maximum number of managed instances that can be registered using this activation.
--
-- /Note:/ Consider using 'registrationLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRegistrationLimit :: Lens.Lens' Activation (Core.Maybe Core.Natural)
aRegistrationLimit = Lens.field @"registrationLimit"
{-# DEPRECATED aRegistrationLimit "Use generic-lens or generic-optics with 'registrationLimit' instead." #-}

-- | The number of managed instances already registered with this activation.
--
-- /Note:/ Consider using 'registrationsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRegistrationsCount :: Lens.Lens' Activation (Core.Maybe Core.Natural)
aRegistrationsCount = Lens.field @"registrationsCount"
{-# DEPRECATED aRegistrationsCount "Use generic-lens or generic-optics with 'registrationsCount' instead." #-}

-- | Tags assigned to the activation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTags :: Lens.Lens' Activation (Core.Maybe [Types.Tag])
aTags = Lens.field @"tags"
{-# DEPRECATED aTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Activation where
  parseJSON =
    Core.withObject "Activation" Core.$
      \x ->
        Activation'
          Core.<$> (x Core..:? "ActivationId")
          Core.<*> (x Core..:? "CreatedDate")
          Core.<*> (x Core..:? "DefaultInstanceName")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "ExpirationDate")
          Core.<*> (x Core..:? "Expired")
          Core.<*> (x Core..:? "IamRole")
          Core.<*> (x Core..:? "RegistrationLimit")
          Core.<*> (x Core..:? "RegistrationsCount")
          Core.<*> (x Core..:? "Tags")
