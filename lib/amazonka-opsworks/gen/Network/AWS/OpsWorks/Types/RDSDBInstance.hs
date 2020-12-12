{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RDSDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.RDSDBInstance
  ( RDSDBInstance (..),

    -- * Smart constructor
    mkRDSDBInstance,

    -- * Lenses
    rdiRDSDBInstanceARN,
    rdiDBUser,
    rdiMissingOnRDS,
    rdiEngine,
    rdiAddress,
    rdiDBInstanceIdentifier,
    rdiRegion,
    rdiStackId,
    rdiDBPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'mkRDSDBInstance' smart constructor.
data RDSDBInstance = RDSDBInstance'
  { rdsDBInstanceARN ::
      Lude.Maybe Lude.Text,
    dbUser :: Lude.Maybe Lude.Text,
    missingOnRDS :: Lude.Maybe Lude.Bool,
    engine :: Lude.Maybe Lude.Text,
    address :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    dbPassword :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RDSDBInstance' with the minimum fields required to make a request.
--
-- * 'address' - The instance's address.
-- * 'dbInstanceIdentifier' - The DB instance identifier.
-- * 'dbPassword' - AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
-- * 'dbUser' - The master user name.
-- * 'engine' - The instance's database engine.
-- * 'missingOnRDS' - Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
-- * 'rdsDBInstanceARN' - The instance's ARN.
-- * 'region' - The instance's AWS region.
-- * 'stackId' - The ID of the stack with which the instance is registered.
mkRDSDBInstance ::
  RDSDBInstance
mkRDSDBInstance =
  RDSDBInstance'
    { rdsDBInstanceARN = Lude.Nothing,
      dbUser = Lude.Nothing,
      missingOnRDS = Lude.Nothing,
      engine = Lude.Nothing,
      address = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      region = Lude.Nothing,
      stackId = Lude.Nothing,
      dbPassword = Lude.Nothing
    }

-- | The instance's ARN.
--
-- /Note:/ Consider using 'rdsDBInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiRDSDBInstanceARN :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiRDSDBInstanceARN = Lens.lens (rdsDBInstanceARN :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {rdsDBInstanceARN = a} :: RDSDBInstance)
{-# DEPRECATED rdiRDSDBInstanceARN "Use generic-lens or generic-optics with 'rdsDBInstanceARN' instead." #-}

-- | The master user name.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDBUser :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiDBUser = Lens.lens (dbUser :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbUser = a} :: RDSDBInstance)
{-# DEPRECATED rdiDBUser "Use generic-lens or generic-optics with 'dbUser' instead." #-}

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
--
-- /Note:/ Consider using 'missingOnRDS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiMissingOnRDS :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Bool)
rdiMissingOnRDS = Lens.lens (missingOnRDS :: RDSDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {missingOnRDS = a} :: RDSDBInstance)
{-# DEPRECATED rdiMissingOnRDS "Use generic-lens or generic-optics with 'missingOnRDS' instead." #-}

-- | The instance's database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiEngine :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiEngine = Lens.lens (engine :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: RDSDBInstance)
{-# DEPRECATED rdiEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The instance's address.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiAddress :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiAddress = Lens.lens (address :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: RDSDBInstance)
{-# DEPRECATED rdiAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The DB instance identifier.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDBInstanceIdentifier :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: RDSDBInstance)
{-# DEPRECATED rdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The instance's AWS region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiRegion :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiRegion = Lens.lens (region :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: RDSDBInstance)
{-# DEPRECATED rdiRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The ID of the stack with which the instance is registered.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiStackId :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiStackId = Lens.lens (stackId :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: RDSDBInstance)
{-# DEPRECATED rdiStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDBPassword :: Lens.Lens' RDSDBInstance (Lude.Maybe Lude.Text)
rdiDBPassword = Lens.lens (dbPassword :: RDSDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbPassword = a} :: RDSDBInstance)
{-# DEPRECATED rdiDBPassword "Use generic-lens or generic-optics with 'dbPassword' instead." #-}

instance Lude.FromJSON RDSDBInstance where
  parseJSON =
    Lude.withObject
      "RDSDBInstance"
      ( \x ->
          RDSDBInstance'
            Lude.<$> (x Lude..:? "RdsDbInstanceArn")
            Lude.<*> (x Lude..:? "DbUser")
            Lude.<*> (x Lude..:? "MissingOnRds")
            Lude.<*> (x Lude..:? "Engine")
            Lude.<*> (x Lude..:? "Address")
            Lude.<*> (x Lude..:? "DbInstanceIdentifier")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "DbPassword")
      )
