{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    replicationInstanceEngineMinimumVersion :: Prelude.Maybe Prelude.Text,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
    -- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
    -- @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Prelude.Maybe ReplicationEndpointTypeValue,
    -- | Indicates if Change Data Capture (CDC) is supported.
    supportsCDC :: Prelude.Maybe Prelude.Bool,
    -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
    engineDisplayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      engineName = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      supportsCDC = Prelude.Nothing,
      engineDisplayName = Prelude.Nothing
    }

-- | The earliest AWS DMS engine version that supports this endpoint engine.
-- Note that endpoint engines released with AWS DMS versions earlier than
-- 3.1.1 do not return a value for this parameter.
supportedEndpointType_replicationInstanceEngineMinimumVersion :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_replicationInstanceEngineMinimumVersion = Lens.lens (\SupportedEndpointType' {replicationInstanceEngineMinimumVersion} -> replicationInstanceEngineMinimumVersion) (\s@SupportedEndpointType' {} a -> s {replicationInstanceEngineMinimumVersion = a} :: SupportedEndpointType)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
supportedEndpointType_engineName :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_engineName = Lens.lens (\SupportedEndpointType' {engineName} -> engineName) (\s@SupportedEndpointType' {} a -> s {engineName = a} :: SupportedEndpointType)

-- | The type of endpoint. Valid values are @source@ and @target@.
supportedEndpointType_endpointType :: Lens.Lens' SupportedEndpointType (Prelude.Maybe ReplicationEndpointTypeValue)
supportedEndpointType_endpointType = Lens.lens (\SupportedEndpointType' {endpointType} -> endpointType) (\s@SupportedEndpointType' {} a -> s {endpointType = a} :: SupportedEndpointType)

-- | Indicates if Change Data Capture (CDC) is supported.
supportedEndpointType_supportsCDC :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Bool)
supportedEndpointType_supportsCDC = Lens.lens (\SupportedEndpointType' {supportsCDC} -> supportsCDC) (\s@SupportedEndpointType' {} a -> s {supportsCDC = a} :: SupportedEndpointType)

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
supportedEndpointType_engineDisplayName :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_engineDisplayName = Lens.lens (\SupportedEndpointType' {engineDisplayName} -> engineDisplayName) (\s@SupportedEndpointType' {} a -> s {engineDisplayName = a} :: SupportedEndpointType)

instance Prelude.FromJSON SupportedEndpointType where
  parseJSON =
    Prelude.withObject
      "SupportedEndpointType"
      ( \x ->
          SupportedEndpointType'
            Prelude.<$> ( x
                            Prelude..:? "ReplicationInstanceEngineMinimumVersion"
                        )
            Prelude.<*> (x Prelude..:? "EngineName")
            Prelude.<*> (x Prelude..:? "EndpointType")
            Prelude.<*> (x Prelude..:? "SupportsCDC")
            Prelude.<*> (x Prelude..:? "EngineDisplayName")
      )

instance Prelude.Hashable SupportedEndpointType

instance Prelude.NFData SupportedEndpointType
