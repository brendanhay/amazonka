{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.CreateDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new instance.
module Amazonka.DocumentDB.CreateDBInstance
  ( -- * Creating a Request
    CreateDBInstance (..),
    newCreateDBInstance,

    -- * Request Lenses
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_availabilityZone,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_promotionTier,
    createDBInstance_tags,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstance_dbClusterIdentifier,

    -- * Destructuring the Response
    CreateDBInstanceResponse (..),
    newCreateDBInstanceResponse,

    -- * Response Lenses
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to CreateDBInstance.
--
-- /See:/ 'newCreateDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { -- | This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
    -- does not perform minor version upgrades regardless of the value set.
    --
    -- Default: @false@
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon EC2 Availability Zone that the instance is created in.
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- Amazon Web Services Region.
    --
    -- Example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- Instance. For more information, see
    -- <https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Using Amazon Performance Insights>.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key identifier for encryption of Performance Insights data.
    --
    -- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
    -- for the KMS key.
    --
    -- If you do not specify a value for PerformanceInsightsKMSKeyId, then
    -- Amazon DocumentDB uses your default KMS key. There is a default KMS key
    -- for your Amazon Web Services account. Your Amazon Web Services account
    -- has a different default KMS key for each Amazon Web Services region.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time range each week during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week.
    --
    -- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Amazon DocumentDB replica
    -- is promoted to the primary instance after a failure of the existing
    -- primary instance.
    --
    -- Default: 1
    --
    -- Valid values: 0-15
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | The tags to be assigned to the instance. You can assign up to 10 tags to
    -- an instance.
    tags :: Prelude.Maybe [Tag],
    -- | The instance identifier. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @mydbinstance@
    dbInstanceIdentifier :: Prelude.Text,
    -- | The compute and memory capacity of the instance; for example,
    -- @db.r5.large@.
    dbInstanceClass :: Prelude.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- Valid value: @docdb@
    engine :: Prelude.Text,
    -- | The identifier of the cluster that the instance will belong to.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMinorVersionUpgrade', 'createDBInstance_autoMinorVersionUpgrade' - This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
-- does not perform minor version upgrades regardless of the value set.
--
-- Default: @false@
--
-- 'availabilityZone', 'createDBInstance_availabilityZone' - The Amazon EC2 Availability Zone that the instance is created in.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
--
-- 'copyTagsToSnapshot', 'createDBInstance_copyTagsToSnapshot' - A value that indicates whether to copy tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- 'enablePerformanceInsights', 'createDBInstance_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- Instance. For more information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Using Amazon Performance Insights>.
--
-- 'performanceInsightsKMSKeyId', 'createDBInstance_performanceInsightsKMSKeyId' - The KMS key identifier for encryption of Performance Insights data.
--
-- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
-- for the KMS key.
--
-- If you do not specify a value for PerformanceInsightsKMSKeyId, then
-- Amazon DocumentDB uses your default KMS key. There is a default KMS key
-- for your Amazon Web Services account. Your Amazon Web Services account
-- has a different default KMS key for each Amazon Web Services region.
--
-- 'preferredMaintenanceWindow', 'createDBInstance_preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
--
-- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
--
-- 'promotionTier', 'createDBInstance_promotionTier' - A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
--
-- Default: 1
--
-- Valid values: 0-15
--
-- 'tags', 'createDBInstance_tags' - The tags to be assigned to the instance. You can assign up to 10 tags to
-- an instance.
--
-- 'dbInstanceIdentifier', 'createDBInstance_dbInstanceIdentifier' - The instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
--
-- 'dbInstanceClass', 'createDBInstance_dbInstanceClass' - The compute and memory capacity of the instance; for example,
-- @db.r5.large@.
--
-- 'engine', 'createDBInstance_engine' - The name of the database engine to be used for this instance.
--
-- Valid value: @docdb@
--
-- 'dbClusterIdentifier', 'createDBInstance_dbClusterIdentifier' - The identifier of the cluster that the instance will belong to.
newCreateDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'dbInstanceClass'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  CreateDBInstance
newCreateDBInstance
  pDBInstanceIdentifier_
  pDBInstanceClass_
  pEngine_
  pDBClusterIdentifier_ =
    CreateDBInstance'
      { autoMinorVersionUpgrade =
          Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        promotionTier = Prelude.Nothing,
        tags = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_,
        dbClusterIdentifier = pDBClusterIdentifier_
      }

-- | This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
-- does not perform minor version upgrades regardless of the value set.
--
-- Default: @false@
createDBInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstance)

-- | The Amazon EC2 Availability Zone that the instance is created in.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
createDBInstance_availabilityZone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_availabilityZone = Lens.lens (\CreateDBInstance' {availabilityZone} -> availabilityZone) (\s@CreateDBInstance' {} a -> s {availabilityZone = a} :: CreateDBInstance)

-- | A value that indicates whether to copy tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
createDBInstance_copyTagsToSnapshot :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_copyTagsToSnapshot = Lens.lens (\CreateDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstance' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstance)

-- | A value that indicates whether to enable Performance Insights for the DB
-- Instance. For more information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Using Amazon Performance Insights>.
createDBInstance_enablePerformanceInsights :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enablePerformanceInsights = Lens.lens (\CreateDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstance' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstance)

-- | The KMS key identifier for encryption of Performance Insights data.
--
-- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
-- for the KMS key.
--
-- If you do not specify a value for PerformanceInsightsKMSKeyId, then
-- Amazon DocumentDB uses your default KMS key. There is a default KMS key
-- for your Amazon Web Services account. Your Amazon Web Services account
-- has a different default KMS key for each Amazon Web Services region.
createDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstance)

-- | The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
--
-- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
createDBInstance_preferredMaintenanceWindow :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_preferredMaintenanceWindow = Lens.lens (\CreateDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBInstance)

-- | A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
--
-- Default: 1
--
-- Valid values: 0-15
createDBInstance_promotionTier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_promotionTier = Lens.lens (\CreateDBInstance' {promotionTier} -> promotionTier) (\s@CreateDBInstance' {} a -> s {promotionTier = a} :: CreateDBInstance)

-- | The tags to be assigned to the instance. You can assign up to 10 tags to
-- an instance.
createDBInstance_tags :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Tag])
createDBInstance_tags = Lens.lens (\CreateDBInstance' {tags} -> tags) (\s@CreateDBInstance' {} a -> s {tags = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The instance identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
createDBInstance_dbInstanceIdentifier :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceIdentifier = Lens.lens (\CreateDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@CreateDBInstance' {} a -> s {dbInstanceIdentifier = a} :: CreateDBInstance)

-- | The compute and memory capacity of the instance; for example,
-- @db.r5.large@.
createDBInstance_dbInstanceClass :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceClass = Lens.lens (\CreateDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstance' {} a -> s {dbInstanceClass = a} :: CreateDBInstance)

-- | The name of the database engine to be used for this instance.
--
-- Valid value: @docdb@
createDBInstance_engine :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_engine = Lens.lens (\CreateDBInstance' {engine} -> engine) (\s@CreateDBInstance' {} a -> s {engine = a} :: CreateDBInstance)

-- | The identifier of the cluster that the instance will belong to.
createDBInstance_dbClusterIdentifier :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbClusterIdentifier = Lens.lens (\CreateDBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBInstance' {} a -> s {dbClusterIdentifier = a} :: CreateDBInstance)

instance Core.AWSRequest CreateDBInstance where
  type
    AWSResponse CreateDBInstance =
      CreateDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceResult"
      ( \s h x ->
          CreateDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstance where
  hashWithSalt _salt CreateDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData CreateDBInstance where
  rnf CreateDBInstance' {..} =
    Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf enablePerformanceInsights
      `Prelude.seq` Prelude.rnf performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf promotionTier
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders CreateDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBInstance where
  toQuery CreateDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AvailabilityZone" Data.=: availabilityZone,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PromotionTier" Data.=: promotionTier,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "Engine" Data.=: engine,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newCreateDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'createDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'createDBInstanceResponse_httpStatus' - The response's http status code.
newCreateDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBInstanceResponse
newCreateDBInstanceResponse pHttpStatus_ =
  CreateDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBInstanceResponse_dbInstance :: Lens.Lens' CreateDBInstanceResponse (Prelude.Maybe DBInstance)
createDBInstanceResponse_dbInstance = Lens.lens (\CreateDBInstanceResponse' {dbInstance} -> dbInstance) (\s@CreateDBInstanceResponse' {} a -> s {dbInstance = a} :: CreateDBInstanceResponse)

-- | The response's http status code.
createDBInstanceResponse_httpStatus :: Lens.Lens' CreateDBInstanceResponse Prelude.Int
createDBInstanceResponse_httpStatus = Lens.lens (\CreateDBInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateDBInstanceResponse' {} a -> s {httpStatus = a} :: CreateDBInstanceResponse)

instance Prelude.NFData CreateDBInstanceResponse where
  rnf CreateDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
