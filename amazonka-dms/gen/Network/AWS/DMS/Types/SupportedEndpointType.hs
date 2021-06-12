{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SupportedEndpointType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SupportedEndpointType where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import qualified Network.AWS.Lens as Lens

-- | Provides information about types of supported endpoints in response to a
-- request by the @DescribeEndpointTypes@ operation. This information
-- includes the type of endpoint, the database engine name, and whether
-- change data capture (CDC) is supported.
--
-- /See:/ 'newSupportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { -- | The earliest AWS DMS engine version that supports this endpoint engine.
    -- Note that endpoint engines released with AWS DMS versions earlier than
    -- 3.1.1 do not return a value for this parameter.
    replicationInstanceEngineMinimumVersion :: Core.Maybe Core.Text,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
    -- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
    -- @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Core.Maybe Core.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Core.Maybe ReplicationEndpointTypeValue,
    -- | Indicates if Change Data Capture (CDC) is supported.
    supportsCDC :: Core.Maybe Core.Bool,
    -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
    engineDisplayName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SupportedEndpointType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstanceEngineMinimumVersion', 'supportedEndpointType_replicationInstanceEngineMinimumVersion' - The earliest AWS DMS engine version that supports this endpoint engine.
-- Note that endpoint engines released with AWS DMS versions earlier than
-- 3.1.1 do not return a value for this parameter.
--
-- 'engineName', 'supportedEndpointType_engineName' - The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
--
-- 'endpointType', 'supportedEndpointType_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'supportsCDC', 'supportedEndpointType_supportsCDC' - Indicates if Change Data Capture (CDC) is supported.
--
-- 'engineDisplayName', 'supportedEndpointType_engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
newSupportedEndpointType ::
  SupportedEndpointType
newSupportedEndpointType =
  SupportedEndpointType'
    { replicationInstanceEngineMinimumVersion =
        Core.Nothing,
      engineName = Core.Nothing,
      endpointType = Core.Nothing,
      supportsCDC = Core.Nothing,
      engineDisplayName = Core.Nothing
    }

-- | The earliest AWS DMS engine version that supports this endpoint engine.
-- Note that endpoint engines released with AWS DMS versions earlier than
-- 3.1.1 do not return a value for this parameter.
supportedEndpointType_replicationInstanceEngineMinimumVersion :: Lens.Lens' SupportedEndpointType (Core.Maybe Core.Text)
supportedEndpointType_replicationInstanceEngineMinimumVersion = Lens.lens (\SupportedEndpointType' {replicationInstanceEngineMinimumVersion} -> replicationInstanceEngineMinimumVersion) (\s@SupportedEndpointType' {} a -> s {replicationInstanceEngineMinimumVersion = a} :: SupportedEndpointType)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
supportedEndpointType_engineName :: Lens.Lens' SupportedEndpointType (Core.Maybe Core.Text)
supportedEndpointType_engineName = Lens.lens (\SupportedEndpointType' {engineName} -> engineName) (\s@SupportedEndpointType' {} a -> s {engineName = a} :: SupportedEndpointType)

-- | The type of endpoint. Valid values are @source@ and @target@.
supportedEndpointType_endpointType :: Lens.Lens' SupportedEndpointType (Core.Maybe ReplicationEndpointTypeValue)
supportedEndpointType_endpointType = Lens.lens (\SupportedEndpointType' {endpointType} -> endpointType) (\s@SupportedEndpointType' {} a -> s {endpointType = a} :: SupportedEndpointType)

-- | Indicates if Change Data Capture (CDC) is supported.
supportedEndpointType_supportsCDC :: Lens.Lens' SupportedEndpointType (Core.Maybe Core.Bool)
supportedEndpointType_supportsCDC = Lens.lens (\SupportedEndpointType' {supportsCDC} -> supportsCDC) (\s@SupportedEndpointType' {} a -> s {supportsCDC = a} :: SupportedEndpointType)

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
supportedEndpointType_engineDisplayName :: Lens.Lens' SupportedEndpointType (Core.Maybe Core.Text)
supportedEndpointType_engineDisplayName = Lens.lens (\SupportedEndpointType' {engineDisplayName} -> engineDisplayName) (\s@SupportedEndpointType' {} a -> s {engineDisplayName = a} :: SupportedEndpointType)

instance Core.FromJSON SupportedEndpointType where
  parseJSON =
    Core.withObject
      "SupportedEndpointType"
      ( \x ->
          SupportedEndpointType'
            Core.<$> ( x
                         Core..:? "ReplicationInstanceEngineMinimumVersion"
                     )
            Core.<*> (x Core..:? "EngineName")
            Core.<*> (x Core..:? "EndpointType")
            Core.<*> (x Core..:? "SupportsCDC")
            Core.<*> (x Core..:? "EngineDisplayName")
      )

instance Core.Hashable SupportedEndpointType

instance Core.NFData SupportedEndpointType
