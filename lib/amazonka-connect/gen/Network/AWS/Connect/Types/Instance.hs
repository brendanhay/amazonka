{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iARN,
    iCreatedTime,
    iOutboundCallsEnabled,
    iInboundCallsEnabled,
    iInstanceAlias,
    iId,
    iInstanceStatus,
    iIdentityManagementType,
    iStatusReason,
    iServiceRole,
  )
where

import Network.AWS.Connect.Types.DirectoryType
import Network.AWS.Connect.Types.InstanceStatus
import Network.AWS.Connect.Types.InstanceStatusReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Connect instance.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Lude.Maybe Lude.Text,
    -- | When the instance was created.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Lude.Maybe Lude.Bool,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Lude.Maybe Lude.Bool,
    -- | The alias of instance.
    instanceAlias :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The identifier of the Amazon Connect instance.
    id :: Lude.Maybe Lude.Text,
    -- | The state of the instance.
    instanceStatus :: Lude.Maybe InstanceStatus,
    -- | The identity management type.
    identityManagementType :: Lude.Maybe DirectoryType,
    -- | Relevant details why the instance was not successfully created.
    statusReason :: Lude.Maybe InstanceStatusReason,
    -- | The service role of the instance.
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance.
-- * 'createdTime' - When the instance was created.
-- * 'outboundCallsEnabled' - Whether outbound calls are enabled.
-- * 'inboundCallsEnabled' - Whether inbound calls are enabled.
-- * 'instanceAlias' - The alias of instance.
-- * 'id' - The identifier of the Amazon Connect instance.
-- * 'instanceStatus' - The state of the instance.
-- * 'identityManagementType' - The identity management type.
-- * 'statusReason' - Relevant details why the instance was not successfully created.
-- * 'serviceRole' - The service role of the instance.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      outboundCallsEnabled = Lude.Nothing,
      inboundCallsEnabled = Lude.Nothing,
      instanceAlias = Lude.Nothing,
      id = Lude.Nothing,
      instanceStatus = Lude.Nothing,
      identityManagementType = Lude.Nothing,
      statusReason = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iARN :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iARN = Lens.lens (arn :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Instance)
{-# DEPRECATED iARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the instance was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedTime :: Lens.Lens' Instance (Lude.Maybe Lude.Timestamp)
iCreatedTime = Lens.lens (createdTime :: Instance -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: Instance)
{-# DEPRECATED iCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Whether outbound calls are enabled.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOutboundCallsEnabled :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
iOutboundCallsEnabled = Lens.lens (outboundCallsEnabled :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {outboundCallsEnabled = a} :: Instance)
{-# DEPRECATED iOutboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead." #-}

-- | Whether inbound calls are enabled.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInboundCallsEnabled :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
iInboundCallsEnabled = Lens.lens (inboundCallsEnabled :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {inboundCallsEnabled = a} :: Instance)
{-# DEPRECATED iInboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead." #-}

-- | The alias of instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceAlias :: Lens.Lens' Instance (Lude.Maybe (Lude.Sensitive Lude.Text))
iInstanceAlias = Lens.lens (instanceAlias :: Instance -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {instanceAlias = a} :: Instance)
{-# DEPRECATED iInstanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iId = Lens.lens (id :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Instance)
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The state of the instance.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceStatus :: Lens.Lens' Instance (Lude.Maybe InstanceStatus)
iInstanceStatus = Lens.lens (instanceStatus :: Instance -> Lude.Maybe InstanceStatus) (\s a -> s {instanceStatus = a} :: Instance)
{-# DEPRECATED iInstanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead." #-}

-- | The identity management type.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIdentityManagementType :: Lens.Lens' Instance (Lude.Maybe DirectoryType)
iIdentityManagementType = Lens.lens (identityManagementType :: Instance -> Lude.Maybe DirectoryType) (\s a -> s {identityManagementType = a} :: Instance)
{-# DEPRECATED iIdentityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead." #-}

-- | Relevant details why the instance was not successfully created.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatusReason :: Lens.Lens' Instance (Lude.Maybe InstanceStatusReason)
iStatusReason = Lens.lens (statusReason :: Instance -> Lude.Maybe InstanceStatusReason) (\s a -> s {statusReason = a} :: Instance)
{-# DEPRECATED iStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The service role of the instance.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iServiceRole :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iServiceRole = Lens.lens (serviceRole :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Instance)
{-# DEPRECATED iServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON Instance where
  parseJSON =
    Lude.withObject
      "Instance"
      ( \x ->
          Instance'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "OutboundCallsEnabled")
            Lude.<*> (x Lude..:? "InboundCallsEnabled")
            Lude.<*> (x Lude..:? "InstanceAlias")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "InstanceStatus")
            Lude.<*> (x Lude..:? "IdentityManagementType")
            Lude.<*> (x Lude..:? "StatusReason")
            Lude.<*> (x Lude..:? "ServiceRole")
      )
