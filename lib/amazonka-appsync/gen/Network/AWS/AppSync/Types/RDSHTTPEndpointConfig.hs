{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RDSHTTPEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RDSHTTPEndpointConfig
  ( RDSHTTPEndpointConfig (..),

    -- * Smart constructor
    mkRDSHTTPEndpointConfig,

    -- * Lenses
    rhttpecDbClusterIdentifier,
    rhttpecSchema,
    rhttpecDatabaseName,
    rhttpecAwsRegion,
    rhttpecAwsSecretStoreARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon RDS HTTP endpoint configuration.
--
-- /See:/ 'mkRDSHTTPEndpointConfig' smart constructor.
data RDSHTTPEndpointConfig = RDSHTTPEndpointConfig'
  { dbClusterIdentifier ::
      Lude.Maybe Lude.Text,
    schema :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Maybe Lude.Text,
    awsRegion :: Lude.Maybe Lude.Text,
    awsSecretStoreARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RDSHTTPEndpointConfig' with the minimum fields required to make a request.
--
-- * 'awsRegion' - AWS Region for RDS HTTP endpoint.
-- * 'awsSecretStoreARN' - AWS secret store ARN for database credentials.
-- * 'databaseName' - Logical database name.
-- * 'dbClusterIdentifier' - Amazon RDS cluster ARN.
-- * 'schema' - Logical schema name.
mkRDSHTTPEndpointConfig ::
  RDSHTTPEndpointConfig
mkRDSHTTPEndpointConfig =
  RDSHTTPEndpointConfig'
    { dbClusterIdentifier = Lude.Nothing,
      schema = Lude.Nothing,
      databaseName = Lude.Nothing,
      awsRegion = Lude.Nothing,
      awsSecretStoreARN = Lude.Nothing
    }

-- | Amazon RDS cluster ARN.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhttpecDbClusterIdentifier :: Lens.Lens' RDSHTTPEndpointConfig (Lude.Maybe Lude.Text)
rhttpecDbClusterIdentifier = Lens.lens (dbClusterIdentifier :: RDSHTTPEndpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RDSHTTPEndpointConfig)
{-# DEPRECATED rhttpecDbClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | Logical schema name.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhttpecSchema :: Lens.Lens' RDSHTTPEndpointConfig (Lude.Maybe Lude.Text)
rhttpecSchema = Lens.lens (schema :: RDSHTTPEndpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {schema = a} :: RDSHTTPEndpointConfig)
{-# DEPRECATED rhttpecSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | Logical database name.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhttpecDatabaseName :: Lens.Lens' RDSHTTPEndpointConfig (Lude.Maybe Lude.Text)
rhttpecDatabaseName = Lens.lens (databaseName :: RDSHTTPEndpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: RDSHTTPEndpointConfig)
{-# DEPRECATED rhttpecDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | AWS Region for RDS HTTP endpoint.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhttpecAwsRegion :: Lens.Lens' RDSHTTPEndpointConfig (Lude.Maybe Lude.Text)
rhttpecAwsRegion = Lens.lens (awsRegion :: RDSHTTPEndpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {awsRegion = a} :: RDSHTTPEndpointConfig)
{-# DEPRECATED rhttpecAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | AWS secret store ARN for database credentials.
--
-- /Note:/ Consider using 'awsSecretStoreARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rhttpecAwsSecretStoreARN :: Lens.Lens' RDSHTTPEndpointConfig (Lude.Maybe Lude.Text)
rhttpecAwsSecretStoreARN = Lens.lens (awsSecretStoreARN :: RDSHTTPEndpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {awsSecretStoreARN = a} :: RDSHTTPEndpointConfig)
{-# DEPRECATED rhttpecAwsSecretStoreARN "Use generic-lens or generic-optics with 'awsSecretStoreARN' instead." #-}

instance Lude.FromJSON RDSHTTPEndpointConfig where
  parseJSON =
    Lude.withObject
      "RDSHTTPEndpointConfig"
      ( \x ->
          RDSHTTPEndpointConfig'
            Lude.<$> (x Lude..:? "dbClusterIdentifier")
            Lude.<*> (x Lude..:? "schema")
            Lude.<*> (x Lude..:? "databaseName")
            Lude.<*> (x Lude..:? "awsRegion")
            Lude.<*> (x Lude..:? "awsSecretStoreArn")
      )

instance Lude.ToJSON RDSHTTPEndpointConfig where
  toJSON RDSHTTPEndpointConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dbClusterIdentifier" Lude..=) Lude.<$> dbClusterIdentifier,
            ("schema" Lude..=) Lude.<$> schema,
            ("databaseName" Lude..=) Lude.<$> databaseName,
            ("awsRegion" Lude..=) Lude.<$> awsRegion,
            ("awsSecretStoreArn" Lude..=) Lude.<$> awsSecretStoreARN
          ]
      )
