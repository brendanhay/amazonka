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
-- Module      : Amazonka.DMS.Types.SupportedEndpointType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.SupportedEndpointType where

import qualified Amazonka.Core as Core
import Amazonka.DMS.Types.ReplicationEndpointTypeValue
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about types of supported endpoints in response to a
-- request by the @DescribeEndpointTypes@ operation. This information
-- includes the type of endpoint, the database engine name, and whether
-- change data capture (CDC) is supported.
--
-- /See:/ 'newSupportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
    engineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Prelude.Maybe ReplicationEndpointTypeValue,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
    -- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
    -- @\"sqlserver\"@, and @\"neptune\"@.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | The earliest DMS engine version that supports this endpoint engine. Note
    -- that endpoint engines released with DMS versions earlier than 3.1.1 do
    -- not return a value for this parameter.
    replicationInstanceEngineMinimumVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicates if change data capture (CDC) is supported.
    supportsCDC :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedEndpointType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineDisplayName', 'supportedEndpointType_engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
--
-- 'endpointType', 'supportedEndpointType_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'engineName', 'supportedEndpointType_engineName' - The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
--
-- 'replicationInstanceEngineMinimumVersion', 'supportedEndpointType_replicationInstanceEngineMinimumVersion' - The earliest DMS engine version that supports this endpoint engine. Note
-- that endpoint engines released with DMS versions earlier than 3.1.1 do
-- not return a value for this parameter.
--
-- 'supportsCDC', 'supportedEndpointType_supportsCDC' - Indicates if change data capture (CDC) is supported.
newSupportedEndpointType ::
  SupportedEndpointType
newSupportedEndpointType =
  SupportedEndpointType'
    { engineDisplayName =
        Prelude.Nothing,
      endpointType = Prelude.Nothing,
      engineName = Prelude.Nothing,
      replicationInstanceEngineMinimumVersion =
        Prelude.Nothing,
      supportsCDC = Prelude.Nothing
    }

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora,\" this value would be \"Amazon Aurora MySQL.\"
supportedEndpointType_engineDisplayName :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_engineDisplayName = Lens.lens (\SupportedEndpointType' {engineDisplayName} -> engineDisplayName) (\s@SupportedEndpointType' {} a -> s {engineDisplayName = a} :: SupportedEndpointType)

-- | The type of endpoint. Valid values are @source@ and @target@.
supportedEndpointType_endpointType :: Lens.Lens' SupportedEndpointType (Prelude.Maybe ReplicationEndpointTypeValue)
supportedEndpointType_endpointType = Lens.lens (\SupportedEndpointType' {endpointType} -> endpointType) (\s@SupportedEndpointType' {} a -> s {endpointType = a} :: SupportedEndpointType)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@, @\"mongodb\"@,
-- @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@, @\"documentdb\"@,
-- @\"sqlserver\"@, and @\"neptune\"@.
supportedEndpointType_engineName :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_engineName = Lens.lens (\SupportedEndpointType' {engineName} -> engineName) (\s@SupportedEndpointType' {} a -> s {engineName = a} :: SupportedEndpointType)

-- | The earliest DMS engine version that supports this endpoint engine. Note
-- that endpoint engines released with DMS versions earlier than 3.1.1 do
-- not return a value for this parameter.
supportedEndpointType_replicationInstanceEngineMinimumVersion :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_replicationInstanceEngineMinimumVersion = Lens.lens (\SupportedEndpointType' {replicationInstanceEngineMinimumVersion} -> replicationInstanceEngineMinimumVersion) (\s@SupportedEndpointType' {} a -> s {replicationInstanceEngineMinimumVersion = a} :: SupportedEndpointType)

-- | Indicates if change data capture (CDC) is supported.
supportedEndpointType_supportsCDC :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Bool)
supportedEndpointType_supportsCDC = Lens.lens (\SupportedEndpointType' {supportsCDC} -> supportsCDC) (\s@SupportedEndpointType' {} a -> s {supportsCDC = a} :: SupportedEndpointType)

instance Core.FromJSON SupportedEndpointType where
  parseJSON =
    Core.withObject
      "SupportedEndpointType"
      ( \x ->
          SupportedEndpointType'
            Prelude.<$> (x Core..:? "EngineDisplayName")
            Prelude.<*> (x Core..:? "EndpointType")
            Prelude.<*> (x Core..:? "EngineName")
            Prelude.<*> ( x
                            Core..:? "ReplicationInstanceEngineMinimumVersion"
                        )
            Prelude.<*> (x Core..:? "SupportsCDC")
      )

instance Prelude.Hashable SupportedEndpointType

instance Prelude.NFData SupportedEndpointType
