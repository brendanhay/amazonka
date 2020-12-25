{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethod
  ( ContactMethod (..),

    -- * Smart constructor
    mkContactMethod,

    -- * Lenses
    cmArn,
    cmContactEndpoint,
    cmCreatedAt,
    cmLocation,
    cmName,
    cmProtocol,
    cmResourceType,
    cmStatus,
    cmSupportCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ContactMethodStatus as Types
import qualified Network.AWS.Lightsail.Types.ContactProtocol as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a contact method.
--
-- A contact method is a way to send you notifications. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
-- /See:/ 'mkContactMethod' smart constructor.
data ContactMethod = ContactMethod'
  { -- | The Amazon Resource Name (ARN) of the contact method.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The destination of the contact method, such as an email address or a mobile phone number.
    contactEndpoint :: Core.Maybe Types.NonEmptyString,
    -- | The timestamp when the contact method was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    location :: Core.Maybe Types.ResourceLocation,
    -- | The name of the contact method.
    name :: Core.Maybe Types.ResourceName,
    -- | The protocol of the contact method, such as email or SMS (text messaging).
    protocol :: Core.Maybe Types.ContactProtocol,
    -- | The Lightsail resource type (e.g., @ContactMethod@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The current status of the contact method.
    --
    -- A contact method has the following possible status:
    --
    --     * @PendingVerification@ - The contact method has not yet been verified, and the verification has not yet expired.
    --
    --
    --     * @Valid@ - The contact method has been verified.
    --
    --
    --     * @InValid@ - An attempt was made to verify the contact method, but the verification has expired.
    status :: Core.Maybe Types.ContactMethodStatus,
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail contact method. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ContactMethod' value with any optional fields omitted.
mkContactMethod ::
  ContactMethod
mkContactMethod =
  ContactMethod'
    { arn = Core.Nothing,
      contactEndpoint = Core.Nothing,
      createdAt = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      protocol = Core.Nothing,
      resourceType = Core.Nothing,
      status = Core.Nothing,
      supportCode = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact method.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmArn :: Lens.Lens' ContactMethod (Core.Maybe Types.NonEmptyString)
cmArn = Lens.field @"arn"
{-# DEPRECATED cmArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The destination of the contact method, such as an email address or a mobile phone number.
--
-- /Note:/ Consider using 'contactEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContactEndpoint :: Lens.Lens' ContactMethod (Core.Maybe Types.NonEmptyString)
cmContactEndpoint = Lens.field @"contactEndpoint"
{-# DEPRECATED cmContactEndpoint "Use generic-lens or generic-optics with 'contactEndpoint' instead." #-}

-- | The timestamp when the contact method was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreatedAt :: Lens.Lens' ContactMethod (Core.Maybe Core.NominalDiffTime)
cmCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED cmCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmLocation :: Lens.Lens' ContactMethod (Core.Maybe Types.ResourceLocation)
cmLocation = Lens.field @"location"
{-# DEPRECATED cmLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the contact method.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' ContactMethod (Core.Maybe Types.ResourceName)
cmName = Lens.field @"name"
{-# DEPRECATED cmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocol of the contact method, such as email or SMS (text messaging).
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmProtocol :: Lens.Lens' ContactMethod (Core.Maybe Types.ContactProtocol)
cmProtocol = Lens.field @"protocol"
{-# DEPRECATED cmProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The Lightsail resource type (e.g., @ContactMethod@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmResourceType :: Lens.Lens' ContactMethod (Core.Maybe Types.ResourceType)
cmResourceType = Lens.field @"resourceType"
{-# DEPRECATED cmResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The current status of the contact method.
--
-- A contact method has the following possible status:
--
--     * @PendingVerification@ - The contact method has not yet been verified, and the verification has not yet expired.
--
--
--     * @Valid@ - The contact method has been verified.
--
--
--     * @InValid@ - An attempt was made to verify the contact method, but the verification has expired.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmStatus :: Lens.Lens' ContactMethod (Core.Maybe Types.ContactMethodStatus)
cmStatus = Lens.field @"status"
{-# DEPRECATED cmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail contact method. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmSupportCode :: Lens.Lens' ContactMethod (Core.Maybe Types.String)
cmSupportCode = Lens.field @"supportCode"
{-# DEPRECATED cmSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

instance Core.FromJSON ContactMethod where
  parseJSON =
    Core.withObject "ContactMethod" Core.$
      \x ->
        ContactMethod'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "contactEndpoint")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "protocol")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "supportCode")
