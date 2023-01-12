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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.SupportedEndpointType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.ReplicationEndpointTypeValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about types of supported endpoints in response to a
-- request by the @DescribeEndpointTypes@ operation. This information
-- includes the type of endpoint, the database engine name, and whether
-- change data capture (CDC) is supported.
--
-- /See:/ 'newSupportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { -- | The type of endpoint. Valid values are @source@ and @target@.
    endpointType :: Prelude.Maybe ReplicationEndpointTypeValue,
    -- | The expanded name for the engine name. For example, if the @EngineName@
    -- parameter is \"aurora\", this value would be \"Amazon Aurora MySQL\".
    engineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The database engine name. Valid values, depending on the EndpointType,
    -- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
    -- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
    -- @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
    -- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
    -- @\"documentdb\"@, @\"sqlserver\"@, @\"neptune\"@, and @\"babelfish\"@.
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
-- 'endpointType', 'supportedEndpointType_endpointType' - The type of endpoint. Valid values are @source@ and @target@.
--
-- 'engineDisplayName', 'supportedEndpointType_engineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora\", this value would be \"Amazon Aurora MySQL\".
--
-- 'engineName', 'supportedEndpointType_engineName' - The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, @\"neptune\"@, and @\"babelfish\"@.
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
    { endpointType =
        Prelude.Nothing,
      engineDisplayName = Prelude.Nothing,
      engineName = Prelude.Nothing,
      replicationInstanceEngineMinimumVersion =
        Prelude.Nothing,
      supportsCDC = Prelude.Nothing
    }

-- | The type of endpoint. Valid values are @source@ and @target@.
supportedEndpointType_endpointType :: Lens.Lens' SupportedEndpointType (Prelude.Maybe ReplicationEndpointTypeValue)
supportedEndpointType_endpointType = Lens.lens (\SupportedEndpointType' {endpointType} -> endpointType) (\s@SupportedEndpointType' {} a -> s {endpointType = a} :: SupportedEndpointType)

-- | The expanded name for the engine name. For example, if the @EngineName@
-- parameter is \"aurora\", this value would be \"Amazon Aurora MySQL\".
supportedEndpointType_engineDisplayName :: Lens.Lens' SupportedEndpointType (Prelude.Maybe Prelude.Text)
supportedEndpointType_engineDisplayName = Lens.lens (\SupportedEndpointType' {engineDisplayName} -> engineDisplayName) (\s@SupportedEndpointType' {} a -> s {engineDisplayName = a} :: SupportedEndpointType)

-- | The database engine name. Valid values, depending on the EndpointType,
-- include @\"mysql\"@, @\"oracle\"@, @\"postgres\"@, @\"mariadb\"@,
-- @\"aurora\"@, @\"aurora-postgresql\"@, @\"redshift\"@, @\"s3\"@,
-- @\"db2\"@, @\"db2-zos\"@, @\"azuredb\"@, @\"sybase\"@, @\"dynamodb\"@,
-- @\"mongodb\"@, @\"kinesis\"@, @\"kafka\"@, @\"elasticsearch\"@,
-- @\"documentdb\"@, @\"sqlserver\"@, @\"neptune\"@, and @\"babelfish\"@.
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

instance Data.FromJSON SupportedEndpointType where
  parseJSON =
    Data.withObject
      "SupportedEndpointType"
      ( \x ->
          SupportedEndpointType'
            Prelude.<$> (x Data..:? "EndpointType")
            Prelude.<*> (x Data..:? "EngineDisplayName")
            Prelude.<*> (x Data..:? "EngineName")
            Prelude.<*> ( x
                            Data..:? "ReplicationInstanceEngineMinimumVersion"
                        )
            Prelude.<*> (x Data..:? "SupportsCDC")
      )

instance Prelude.Hashable SupportedEndpointType where
  hashWithSalt _salt SupportedEndpointType' {..} =
    _salt `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` engineDisplayName
      `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` replicationInstanceEngineMinimumVersion
      `Prelude.hashWithSalt` supportsCDC

instance Prelude.NFData SupportedEndpointType where
  rnf SupportedEndpointType' {..} =
    Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf engineDisplayName
      `Prelude.seq` Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf replicationInstanceEngineMinimumVersion
      `Prelude.seq` Prelude.rnf supportsCDC
