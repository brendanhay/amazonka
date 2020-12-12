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
    cmStatus,
    cmResourceType,
    cmArn,
    cmCreatedAt,
    cmLocation,
    cmProtocol,
    cmName,
    cmSupportCode,
    cmContactEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContactMethodStatus
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes a contact method.
--
-- A contact method is a way to send you notifications. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
-- /See:/ 'mkContactMethod' smart constructor.
data ContactMethod = ContactMethod'
  { status ::
      Lude.Maybe ContactMethodStatus,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    protocol :: Lude.Maybe ContactProtocol,
    name :: Lude.Maybe Lude.Text,
    supportCode :: Lude.Maybe Lude.Text,
    contactEndpoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContactMethod' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the contact method.
-- * 'contactEndpoint' - The destination of the contact method, such as an email address or a mobile phone number.
-- * 'createdAt' - The timestamp when the contact method was created.
-- * 'location' - Undocumented field.
-- * 'name' - The name of the contact method.
-- * 'protocol' - The protocol of the contact method, such as email or SMS (text messaging).
-- * 'resourceType' - The Lightsail resource type (e.g., @ContactMethod@ ).
-- * 'status' - The current status of the contact method.
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
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail contact method. This code enables our support team to look up your Lightsail information more easily.
mkContactMethod ::
  ContactMethod
mkContactMethod =
  ContactMethod'
    { status = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      protocol = Lude.Nothing,
      name = Lude.Nothing,
      supportCode = Lude.Nothing,
      contactEndpoint = Lude.Nothing
    }

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
cmStatus :: Lens.Lens' ContactMethod (Lude.Maybe ContactMethodStatus)
cmStatus = Lens.lens (status :: ContactMethod -> Lude.Maybe ContactMethodStatus) (\s a -> s {status = a} :: ContactMethod)
{-# DEPRECATED cmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Lightsail resource type (e.g., @ContactMethod@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmResourceType :: Lens.Lens' ContactMethod (Lude.Maybe ResourceType)
cmResourceType = Lens.lens (resourceType :: ContactMethod -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ContactMethod)
{-# DEPRECATED cmResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the contact method.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmArn :: Lens.Lens' ContactMethod (Lude.Maybe Lude.Text)
cmArn = Lens.lens (arn :: ContactMethod -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ContactMethod)
{-# DEPRECATED cmArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the contact method was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreatedAt :: Lens.Lens' ContactMethod (Lude.Maybe Lude.Timestamp)
cmCreatedAt = Lens.lens (createdAt :: ContactMethod -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ContactMethod)
{-# DEPRECATED cmCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmLocation :: Lens.Lens' ContactMethod (Lude.Maybe ResourceLocation)
cmLocation = Lens.lens (location :: ContactMethod -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: ContactMethod)
{-# DEPRECATED cmLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The protocol of the contact method, such as email or SMS (text messaging).
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmProtocol :: Lens.Lens' ContactMethod (Lude.Maybe ContactProtocol)
cmProtocol = Lens.lens (protocol :: ContactMethod -> Lude.Maybe ContactProtocol) (\s a -> s {protocol = a} :: ContactMethod)
{-# DEPRECATED cmProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The name of the contact method.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' ContactMethod (Lude.Maybe Lude.Text)
cmName = Lens.lens (name :: ContactMethod -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ContactMethod)
{-# DEPRECATED cmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail contact method. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmSupportCode :: Lens.Lens' ContactMethod (Lude.Maybe Lude.Text)
cmSupportCode = Lens.lens (supportCode :: ContactMethod -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: ContactMethod)
{-# DEPRECATED cmSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The destination of the contact method, such as an email address or a mobile phone number.
--
-- /Note:/ Consider using 'contactEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContactEndpoint :: Lens.Lens' ContactMethod (Lude.Maybe Lude.Text)
cmContactEndpoint = Lens.lens (contactEndpoint :: ContactMethod -> Lude.Maybe Lude.Text) (\s a -> s {contactEndpoint = a} :: ContactMethod)
{-# DEPRECATED cmContactEndpoint "Use generic-lens or generic-optics with 'contactEndpoint' instead." #-}

instance Lude.FromJSON ContactMethod where
  parseJSON =
    Lude.withObject
      "ContactMethod"
      ( \x ->
          ContactMethod'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "contactEndpoint")
      )
