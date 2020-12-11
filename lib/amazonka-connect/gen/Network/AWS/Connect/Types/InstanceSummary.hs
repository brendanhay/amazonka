-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceSummary
  ( InstanceSummary (..),

    -- * Smart constructor
    mkInstanceSummary,

    -- * Lenses
    isARN,
    isCreatedTime,
    isOutboundCallsEnabled,
    isInboundCallsEnabled,
    isInstanceAlias,
    isId,
    isInstanceStatus,
    isIdentityManagementType,
    isServiceRole,
  )
where

import Network.AWS.Connect.Types.DirectoryType
import Network.AWS.Connect.Types.InstanceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the instance.
--
-- /See:/ 'mkInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { arn ::
      Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Timestamp,
    outboundCallsEnabled :: Lude.Maybe Lude.Bool,
    inboundCallsEnabled :: Lude.Maybe Lude.Bool,
    instanceAlias :: Lude.Maybe (Lude.Sensitive Lude.Text),
    id :: Lude.Maybe Lude.Text,
    instanceStatus :: Lude.Maybe InstanceStatus,
    identityManagementType :: Lude.Maybe DirectoryType,
    serviceRole :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the instance.
-- * 'createdTime' - When the instance was created.
-- * 'id' - The identifier of the instance.
-- * 'identityManagementType' - The identity management type of the instance.
-- * 'inboundCallsEnabled' - Whether inbound calls are enabled.
-- * 'instanceAlias' - The alias of the instance.
-- * 'instanceStatus' - The state of the instance.
-- * 'outboundCallsEnabled' - Whether outbound calls are enabled.
-- * 'serviceRole' - The service role of the instance.
mkInstanceSummary ::
  InstanceSummary
mkInstanceSummary =
  InstanceSummary'
    { arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      outboundCallsEnabled = Lude.Nothing,
      inboundCallsEnabled = Lude.Nothing,
      instanceAlias = Lude.Nothing,
      id = Lude.Nothing,
      instanceStatus = Lude.Nothing,
      identityManagementType = Lude.Nothing,
      serviceRole = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isARN :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Text)
isARN = Lens.lens (arn :: InstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: InstanceSummary)
{-# DEPRECATED isARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the instance was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCreatedTime :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Timestamp)
isCreatedTime = Lens.lens (createdTime :: InstanceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: InstanceSummary)
{-# DEPRECATED isCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Whether outbound calls are enabled.
--
-- /Note:/ Consider using 'outboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isOutboundCallsEnabled :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Bool)
isOutboundCallsEnabled = Lens.lens (outboundCallsEnabled :: InstanceSummary -> Lude.Maybe Lude.Bool) (\s a -> s {outboundCallsEnabled = a} :: InstanceSummary)
{-# DEPRECATED isOutboundCallsEnabled "Use generic-lens or generic-optics with 'outboundCallsEnabled' instead." #-}

-- | Whether inbound calls are enabled.
--
-- /Note:/ Consider using 'inboundCallsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInboundCallsEnabled :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Bool)
isInboundCallsEnabled = Lens.lens (inboundCallsEnabled :: InstanceSummary -> Lude.Maybe Lude.Bool) (\s a -> s {inboundCallsEnabled = a} :: InstanceSummary)
{-# DEPRECATED isInboundCallsEnabled "Use generic-lens or generic-optics with 'inboundCallsEnabled' instead." #-}

-- | The alias of the instance.
--
-- /Note:/ Consider using 'instanceAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceAlias :: Lens.Lens' InstanceSummary (Lude.Maybe (Lude.Sensitive Lude.Text))
isInstanceAlias = Lens.lens (instanceAlias :: InstanceSummary -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {instanceAlias = a} :: InstanceSummary)
{-# DEPRECATED isInstanceAlias "Use generic-lens or generic-optics with 'instanceAlias' instead." #-}

-- | The identifier of the instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isId :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Text)
isId = Lens.lens (id :: InstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InstanceSummary)
{-# DEPRECATED isId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The state of the instance.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceStatus :: Lens.Lens' InstanceSummary (Lude.Maybe InstanceStatus)
isInstanceStatus = Lens.lens (instanceStatus :: InstanceSummary -> Lude.Maybe InstanceStatus) (\s a -> s {instanceStatus = a} :: InstanceSummary)
{-# DEPRECATED isInstanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead." #-}

-- | The identity management type of the instance.
--
-- /Note:/ Consider using 'identityManagementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIdentityManagementType :: Lens.Lens' InstanceSummary (Lude.Maybe DirectoryType)
isIdentityManagementType = Lens.lens (identityManagementType :: InstanceSummary -> Lude.Maybe DirectoryType) (\s a -> s {identityManagementType = a} :: InstanceSummary)
{-# DEPRECATED isIdentityManagementType "Use generic-lens or generic-optics with 'identityManagementType' instead." #-}

-- | The service role of the instance.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isServiceRole :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Text)
isServiceRole = Lens.lens (serviceRole :: InstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: InstanceSummary)
{-# DEPRECATED isServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.FromJSON InstanceSummary where
  parseJSON =
    Lude.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "OutboundCallsEnabled")
            Lude.<*> (x Lude..:? "InboundCallsEnabled")
            Lude.<*> (x Lude..:? "InstanceAlias")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "InstanceStatus")
            Lude.<*> (x Lude..:? "IdentityManagementType")
            Lude.<*> (x Lude..:? "ServiceRole")
      )
