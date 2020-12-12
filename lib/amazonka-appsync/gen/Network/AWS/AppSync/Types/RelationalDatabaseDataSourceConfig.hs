{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
  ( RelationalDatabaseDataSourceConfig (..),

    -- * Smart constructor
    mkRelationalDatabaseDataSourceConfig,

    -- * Lenses
    rddscRelationalDatabaseSourceType,
    rddscRdsHTTPEndpointConfig,
  )
where

import Network.AWS.AppSync.Types.RDSHTTPEndpointConfig
import Network.AWS.AppSync.Types.RelationalDatabaseSourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a relational database data source configuration.
--
-- /See:/ 'mkRelationalDatabaseDataSourceConfig' smart constructor.
data RelationalDatabaseDataSourceConfig = RelationalDatabaseDataSourceConfig'
  { relationalDatabaseSourceType ::
      Lude.Maybe
        RelationalDatabaseSourceType,
    rdsHTTPEndpointConfig ::
      Lude.Maybe
        RDSHTTPEndpointConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabaseDataSourceConfig' with the minimum fields required to make a request.
--
-- * 'rdsHTTPEndpointConfig' - Amazon RDS HTTP endpoint settings.
-- * 'relationalDatabaseSourceType' - Source type for the relational database.
--
--
--     * __RDS_HTTP_ENDPOINT__ : The relational database source type is an Amazon RDS HTTP endpoint.
mkRelationalDatabaseDataSourceConfig ::
  RelationalDatabaseDataSourceConfig
mkRelationalDatabaseDataSourceConfig =
  RelationalDatabaseDataSourceConfig'
    { relationalDatabaseSourceType =
        Lude.Nothing,
      rdsHTTPEndpointConfig = Lude.Nothing
    }

-- | Source type for the relational database.
--
--
--     * __RDS_HTTP_ENDPOINT__ : The relational database source type is an Amazon RDS HTTP endpoint.
--
--
--
-- /Note:/ Consider using 'relationalDatabaseSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddscRelationalDatabaseSourceType :: Lens.Lens' RelationalDatabaseDataSourceConfig (Lude.Maybe RelationalDatabaseSourceType)
rddscRelationalDatabaseSourceType = Lens.lens (relationalDatabaseSourceType :: RelationalDatabaseDataSourceConfig -> Lude.Maybe RelationalDatabaseSourceType) (\s a -> s {relationalDatabaseSourceType = a} :: RelationalDatabaseDataSourceConfig)
{-# DEPRECATED rddscRelationalDatabaseSourceType "Use generic-lens or generic-optics with 'relationalDatabaseSourceType' instead." #-}

-- | Amazon RDS HTTP endpoint settings.
--
-- /Note:/ Consider using 'rdsHTTPEndpointConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rddscRdsHTTPEndpointConfig :: Lens.Lens' RelationalDatabaseDataSourceConfig (Lude.Maybe RDSHTTPEndpointConfig)
rddscRdsHTTPEndpointConfig = Lens.lens (rdsHTTPEndpointConfig :: RelationalDatabaseDataSourceConfig -> Lude.Maybe RDSHTTPEndpointConfig) (\s a -> s {rdsHTTPEndpointConfig = a} :: RelationalDatabaseDataSourceConfig)
{-# DEPRECATED rddscRdsHTTPEndpointConfig "Use generic-lens or generic-optics with 'rdsHTTPEndpointConfig' instead." #-}

instance Lude.FromJSON RelationalDatabaseDataSourceConfig where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseDataSourceConfig"
      ( \x ->
          RelationalDatabaseDataSourceConfig'
            Lude.<$> (x Lude..:? "relationalDatabaseSourceType")
            Lude.<*> (x Lude..:? "rdsHttpEndpointConfig")
      )

instance Lude.ToJSON RelationalDatabaseDataSourceConfig where
  toJSON RelationalDatabaseDataSourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("relationalDatabaseSourceType" Lude..=)
              Lude.<$> relationalDatabaseSourceType,
            ("rdsHttpEndpointConfig" Lude..=) Lude.<$> rdsHTTPEndpointConfig
          ]
      )
