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
    aExpired,
    aDefaultInstanceName,
    aActivationId,
    aCreatedDate,
    aRegistrationLimit,
    aExpirationDate,
    aDescription,
    aTags,
    aRegistrationsCount,
    aIAMRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.Tag

-- | An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.
--
-- /See:/ 'mkActivation' smart constructor.
data Activation = Activation'
  { expired :: Lude.Maybe Lude.Bool,
    defaultInstanceName :: Lude.Maybe Lude.Text,
    activationId :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    registrationLimit :: Lude.Maybe Lude.Natural,
    expirationDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    registrationsCount :: Lude.Maybe Lude.Natural,
    iamRole :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Activation' with the minimum fields required to make a request.
--
-- * 'activationId' - The ID created by Systems Manager when you submitted the activation.
-- * 'createdDate' - The date the activation was created.
-- * 'defaultInstanceName' - A name for the managed instance when it is created.
-- * 'description' - A user defined description of the activation.
-- * 'expirationDate' - The date when this activation can no longer be used to register managed instances.
-- * 'expired' - Whether or not the activation is expired.
-- * 'iamRole' - The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
-- * 'registrationLimit' - The maximum number of managed instances that can be registered using this activation.
-- * 'registrationsCount' - The number of managed instances already registered with this activation.
-- * 'tags' - Tags assigned to the activation.
mkActivation ::
  Activation
mkActivation =
  Activation'
    { expired = Lude.Nothing,
      defaultInstanceName = Lude.Nothing,
      activationId = Lude.Nothing,
      createdDate = Lude.Nothing,
      registrationLimit = Lude.Nothing,
      expirationDate = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      registrationsCount = Lude.Nothing,
      iamRole = Lude.Nothing
    }

-- | Whether or not the activation is expired.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aExpired :: Lens.Lens' Activation (Lude.Maybe Lude.Bool)
aExpired = Lens.lens (expired :: Activation -> Lude.Maybe Lude.Bool) (\s a -> s {expired = a} :: Activation)
{-# DEPRECATED aExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | A name for the managed instance when it is created.
--
-- /Note:/ Consider using 'defaultInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDefaultInstanceName :: Lens.Lens' Activation (Lude.Maybe Lude.Text)
aDefaultInstanceName = Lens.lens (defaultInstanceName :: Activation -> Lude.Maybe Lude.Text) (\s a -> s {defaultInstanceName = a} :: Activation)
{-# DEPRECATED aDefaultInstanceName "Use generic-lens or generic-optics with 'defaultInstanceName' instead." #-}

-- | The ID created by Systems Manager when you submitted the activation.
--
-- /Note:/ Consider using 'activationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActivationId :: Lens.Lens' Activation (Lude.Maybe Lude.Text)
aActivationId = Lens.lens (activationId :: Activation -> Lude.Maybe Lude.Text) (\s a -> s {activationId = a} :: Activation)
{-# DEPRECATED aActivationId "Use generic-lens or generic-optics with 'activationId' instead." #-}

-- | The date the activation was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedDate :: Lens.Lens' Activation (Lude.Maybe Lude.Timestamp)
aCreatedDate = Lens.lens (createdDate :: Activation -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: Activation)
{-# DEPRECATED aCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The maximum number of managed instances that can be registered using this activation.
--
-- /Note:/ Consider using 'registrationLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRegistrationLimit :: Lens.Lens' Activation (Lude.Maybe Lude.Natural)
aRegistrationLimit = Lens.lens (registrationLimit :: Activation -> Lude.Maybe Lude.Natural) (\s a -> s {registrationLimit = a} :: Activation)
{-# DEPRECATED aRegistrationLimit "Use generic-lens or generic-optics with 'registrationLimit' instead." #-}

-- | The date when this activation can no longer be used to register managed instances.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aExpirationDate :: Lens.Lens' Activation (Lude.Maybe Lude.Timestamp)
aExpirationDate = Lens.lens (expirationDate :: Activation -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: Activation)
{-# DEPRECATED aExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | A user defined description of the activation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Activation (Lude.Maybe Lude.Text)
aDescription = Lens.lens (description :: Activation -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Activation)
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Tags assigned to the activation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTags :: Lens.Lens' Activation (Lude.Maybe [Tag])
aTags = Lens.lens (tags :: Activation -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Activation)
{-# DEPRECATED aTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The number of managed instances already registered with this activation.
--
-- /Note:/ Consider using 'registrationsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRegistrationsCount :: Lens.Lens' Activation (Lude.Maybe Lude.Natural)
aRegistrationsCount = Lens.lens (registrationsCount :: Activation -> Lude.Maybe Lude.Natural) (\s a -> s {registrationsCount = a} :: Activation)
{-# DEPRECATED aRegistrationsCount "Use generic-lens or generic-optics with 'registrationsCount' instead." #-}

-- | The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIAMRole :: Lens.Lens' Activation (Lude.Maybe Lude.Text)
aIAMRole = Lens.lens (iamRole :: Activation -> Lude.Maybe Lude.Text) (\s a -> s {iamRole = a} :: Activation)
{-# DEPRECATED aIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Lude.FromJSON Activation where
  parseJSON =
    Lude.withObject
      "Activation"
      ( \x ->
          Activation'
            Lude.<$> (x Lude..:? "Expired")
            Lude.<*> (x Lude..:? "DefaultInstanceName")
            Lude.<*> (x Lude..:? "ActivationId")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "RegistrationLimit")
            Lude.<*> (x Lude..:? "ExpirationDate")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RegistrationsCount")
            Lude.<*> (x Lude..:? "IamRole")
      )
