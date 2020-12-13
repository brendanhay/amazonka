{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SupportedEndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SupportedEndpointType
  ( SupportedEndpointType (..),

    -- * Smart constructor
    mkSupportedEndpointType,

    -- * Lenses
    setEngineDisplayName,
    setEndpointType,
    setEngineName,
    setReplicationInstanceEngineMinimumVersion,
    setSupportsCDC,
  )
where

import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about types of supported endpoints in response to a request by the @DescribeEndpointTypes@ operation. This information includes the type of endpoint, the database engine name, and whether change data capture (CDC) is supported.
--
-- /See:/ 'mkSupportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { -- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
    engineDisplayName :: Lude.Maybe Lude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@ .
    endpointType :: Lude.Maybe ReplicationEndpointTypeValue,
    -- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
    engineName :: Lude.Maybe Lude.Text,
    -- | The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
    replicationInstanceEngineMinimumVersion :: Lude.Maybe Lude.Text,
    -- | Indicates if Change Data Capture (CDC) is supported.
    supportsCDC :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SupportedEndpointType' with the minimum fields required to make a request.
--
-- * 'engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
-- * 'endpointType' - The type of endpoint. Valid values are @source@ and @target@ .
-- * 'engineName' - The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
-- * 'replicationInstanceEngineMinimumVersion' - The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
-- * 'supportsCDC' - Indicates if Change Data Capture (CDC) is supported.
mkSupportedEndpointType ::
  SupportedEndpointType
mkSupportedEndpointType =
  SupportedEndpointType'
    { engineDisplayName = Lude.Nothing,
      endpointType = Lude.Nothing,
      engineName = Lude.Nothing,
      replicationInstanceEngineMinimumVersion = Lude.Nothing,
      supportsCDC = Lude.Nothing
    }

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- /Note:/ Consider using 'engineDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEngineDisplayName :: Lens.Lens' SupportedEndpointType (Lude.Maybe Lude.Text)
setEngineDisplayName = Lens.lens (engineDisplayName :: SupportedEndpointType -> Lude.Maybe Lude.Text) (\s a -> s {engineDisplayName = a} :: SupportedEndpointType)
{-# DEPRECATED setEngineDisplayName "Use generic-lens or generic-optics with 'engineDisplayName' instead." #-}

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEndpointType :: Lens.Lens' SupportedEndpointType (Lude.Maybe ReplicationEndpointTypeValue)
setEndpointType = Lens.lens (endpointType :: SupportedEndpointType -> Lude.Maybe ReplicationEndpointTypeValue) (\s a -> s {endpointType = a} :: SupportedEndpointType)
{-# DEPRECATED setEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEngineName :: Lens.Lens' SupportedEndpointType (Lude.Maybe Lude.Text)
setEngineName = Lens.lens (engineName :: SupportedEndpointType -> Lude.Maybe Lude.Text) (\s a -> s {engineName = a} :: SupportedEndpointType)
{-# DEPRECATED setEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
--
-- /Note:/ Consider using 'replicationInstanceEngineMinimumVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setReplicationInstanceEngineMinimumVersion :: Lens.Lens' SupportedEndpointType (Lude.Maybe Lude.Text)
setReplicationInstanceEngineMinimumVersion = Lens.lens (replicationInstanceEngineMinimumVersion :: SupportedEndpointType -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceEngineMinimumVersion = a} :: SupportedEndpointType)
{-# DEPRECATED setReplicationInstanceEngineMinimumVersion "Use generic-lens or generic-optics with 'replicationInstanceEngineMinimumVersion' instead." #-}

-- | Indicates if Change Data Capture (CDC) is supported.
--
-- /Note:/ Consider using 'supportsCDC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setSupportsCDC :: Lens.Lens' SupportedEndpointType (Lude.Maybe Lude.Bool)
setSupportsCDC = Lens.lens (supportsCDC :: SupportedEndpointType -> Lude.Maybe Lude.Bool) (\s a -> s {supportsCDC = a} :: SupportedEndpointType)
{-# DEPRECATED setSupportsCDC "Use generic-lens or generic-optics with 'supportsCDC' instead." #-}

instance Lude.FromJSON SupportedEndpointType where
  parseJSON =
    Lude.withObject
      "SupportedEndpointType"
      ( \x ->
          SupportedEndpointType'
            Lude.<$> (x Lude..:? "EngineDisplayName")
            Lude.<*> (x Lude..:? "EndpointType")
            Lude.<*> (x Lude..:? "EngineName")
            Lude.<*> (x Lude..:? "ReplicationInstanceEngineMinimumVersion")
            Lude.<*> (x Lude..:? "SupportsCDC")
      )
