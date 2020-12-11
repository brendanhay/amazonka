-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceEntry
  ( InstanceEntry (..),

    -- * Smart constructor
    mkInstanceEntry,

    -- * Lenses
    ieUserData,
    ieSourceName,
    ieInstanceType,
    iePortInfoSource,
    ieAvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.PortInfoSourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon Elastic Compute Cloud instance and related resources to be created using the @create cloud formation stack@ operation.
--
-- /See:/ 'mkInstanceEntry' smart constructor.
data InstanceEntry = InstanceEntry'
  { userData ::
      Lude.Maybe Lude.Text,
    sourceName :: Lude.Text,
    instanceType :: Lude.Text,
    portInfoSource :: PortInfoSourceType,
    availabilityZone :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceEntry' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone for the new Amazon EC2 instance.
-- * 'instanceType' - The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
-- * 'portInfoSource' - The port configuration to use for the new Amazon EC2 instance.
--
-- The following configuration options are available:
--
--     * @DEFAULT@ - Use the default firewall settings from the Lightsail instance blueprint.
--
--
--     * @INSTANCE@ - Use the configured firewall settings from the source Lightsail instance.
--
--
--     * @NONE@ - Use the default Amazon EC2 security group.
--
--
--     * @CLOSED@ - All ports closed.
--
--
-- * 'sourceName' - The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance.
--
-- Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
-- * 'userData' - A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
mkInstanceEntry ::
  -- | 'sourceName'
  Lude.Text ->
  -- | 'instanceType'
  Lude.Text ->
  -- | 'portInfoSource'
  PortInfoSourceType ->
  -- | 'availabilityZone'
  Lude.Text ->
  InstanceEntry
mkInstanceEntry
  pSourceName_
  pInstanceType_
  pPortInfoSource_
  pAvailabilityZone_ =
    InstanceEntry'
      { userData = Lude.Nothing,
        sourceName = pSourceName_,
        instanceType = pInstanceType_,
        portInfoSource = pPortInfoSource_,
        availabilityZone = pAvailabilityZone_
      }

-- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieUserData :: Lens.Lens' InstanceEntry (Lude.Maybe Lude.Text)
ieUserData = Lens.lens (userData :: InstanceEntry -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: InstanceEntry)
{-# DEPRECATED ieUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance.
--
-- Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
--
-- /Note:/ Consider using 'sourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieSourceName :: Lens.Lens' InstanceEntry Lude.Text
ieSourceName = Lens.lens (sourceName :: InstanceEntry -> Lude.Text) (\s a -> s {sourceName = a} :: InstanceEntry)
{-# DEPRECATED ieSourceName "Use generic-lens or generic-optics with 'sourceName' instead." #-}

-- | The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieInstanceType :: Lens.Lens' InstanceEntry Lude.Text
ieInstanceType = Lens.lens (instanceType :: InstanceEntry -> Lude.Text) (\s a -> s {instanceType = a} :: InstanceEntry)
{-# DEPRECATED ieInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The port configuration to use for the new Amazon EC2 instance.
--
-- The following configuration options are available:
--
--     * @DEFAULT@ - Use the default firewall settings from the Lightsail instance blueprint.
--
--
--     * @INSTANCE@ - Use the configured firewall settings from the source Lightsail instance.
--
--
--     * @NONE@ - Use the default Amazon EC2 security group.
--
--
--     * @CLOSED@ - All ports closed.
--
--
--
-- /Note:/ Consider using 'portInfoSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iePortInfoSource :: Lens.Lens' InstanceEntry PortInfoSourceType
iePortInfoSource = Lens.lens (portInfoSource :: InstanceEntry -> PortInfoSourceType) (\s a -> s {portInfoSource = a} :: InstanceEntry)
{-# DEPRECATED iePortInfoSource "Use generic-lens or generic-optics with 'portInfoSource' instead." #-}

-- | The Availability Zone for the new Amazon EC2 instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieAvailabilityZone :: Lens.Lens' InstanceEntry Lude.Text
ieAvailabilityZone = Lens.lens (availabilityZone :: InstanceEntry -> Lude.Text) (\s a -> s {availabilityZone = a} :: InstanceEntry)
{-# DEPRECATED ieAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.ToJSON InstanceEntry where
  toJSON InstanceEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("userData" Lude..=) Lude.<$> userData,
            Lude.Just ("sourceName" Lude..= sourceName),
            Lude.Just ("instanceType" Lude..= instanceType),
            Lude.Just ("portInfoSource" Lude..= portInfoSource),
            Lude.Just ("availabilityZone" Lude..= availabilityZone)
          ]
      )
