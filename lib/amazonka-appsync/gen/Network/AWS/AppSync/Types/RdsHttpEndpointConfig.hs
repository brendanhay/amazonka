{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RdsHttpEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.RdsHttpEndpointConfig
  ( RdsHttpEndpointConfig (..)
  -- * Smart constructor
  , mkRdsHttpEndpointConfig
  -- * Lenses
  , rhecAwsRegion
  , rhecAwsSecretStoreArn
  , rhecDatabaseName
  , rhecDbClusterIdentifier
  , rhecSchema
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon RDS HTTP endpoint configuration.
--
-- /See:/ 'mkRdsHttpEndpointConfig' smart constructor.
data RdsHttpEndpointConfig = RdsHttpEndpointConfig'
  { awsRegion :: Core.Maybe Core.Text
    -- ^ AWS Region for RDS HTTP endpoint.
  , awsSecretStoreArn :: Core.Maybe Core.Text
    -- ^ AWS secret store ARN for database credentials.
  , databaseName :: Core.Maybe Core.Text
    -- ^ Logical database name.
  , dbClusterIdentifier :: Core.Maybe Core.Text
    -- ^ Amazon RDS cluster ARN.
  , schema :: Core.Maybe Core.Text
    -- ^ Logical schema name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RdsHttpEndpointConfig' value with any optional fields omitted.
mkRdsHttpEndpointConfig
    :: RdsHttpEndpointConfig
mkRdsHttpEndpointConfig
  = RdsHttpEndpointConfig'{awsRegion = Core.Nothing,
                           awsSecretStoreArn = Core.Nothing, databaseName = Core.Nothing,
                           dbClusterIdentifier = Core.Nothing, schema = Core.Nothing}

-- | AWS Region for RDS HTTP endpoint.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhecAwsRegion :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rhecAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE rhecAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

-- | AWS secret store ARN for database credentials.
--
-- /Note:/ Consider using 'awsSecretStoreArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhecAwsSecretStoreArn :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rhecAwsSecretStoreArn = Lens.field @"awsSecretStoreArn"
{-# INLINEABLE rhecAwsSecretStoreArn #-}
{-# DEPRECATED awsSecretStoreArn "Use generic-lens or generic-optics with 'awsSecretStoreArn' instead"  #-}

-- | Logical database name.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhecDatabaseName :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rhecDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE rhecDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | Amazon RDS cluster ARN.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhecDbClusterIdentifier :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rhecDbClusterIdentifier = Lens.field @"dbClusterIdentifier"
{-# INLINEABLE rhecDbClusterIdentifier #-}
{-# DEPRECATED dbClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead"  #-}

-- | Logical schema name.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhecSchema :: Lens.Lens' RdsHttpEndpointConfig (Core.Maybe Core.Text)
rhecSchema = Lens.field @"schema"
{-# INLINEABLE rhecSchema #-}
{-# DEPRECATED schema "Use generic-lens or generic-optics with 'schema' instead"  #-}

instance Core.FromJSON RdsHttpEndpointConfig where
        toJSON RdsHttpEndpointConfig{..}
          = Core.object
              (Core.catMaybes
                 [("awsRegion" Core..=) Core.<$> awsRegion,
                  ("awsSecretStoreArn" Core..=) Core.<$> awsSecretStoreArn,
                  ("databaseName" Core..=) Core.<$> databaseName,
                  ("dbClusterIdentifier" Core..=) Core.<$> dbClusterIdentifier,
                  ("schema" Core..=) Core.<$> schema])

instance Core.FromJSON RdsHttpEndpointConfig where
        parseJSON
          = Core.withObject "RdsHttpEndpointConfig" Core.$
              \ x ->
                RdsHttpEndpointConfig' Core.<$>
                  (x Core..:? "awsRegion") Core.<*> x Core..:? "awsSecretStoreArn"
                    Core.<*> x Core..:? "databaseName"
                    Core.<*> x Core..:? "dbClusterIdentifier"
                    Core.<*> x Core..:? "schema"
