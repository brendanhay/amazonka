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
-- Module      : Amazonka.DocumentDB.ModifyDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies settings for an instance. You can change one or more database
-- configuration parameters by specifying these parameters and the new
-- values in the request.
module Amazonka.DocumentDB.ModifyDBInstance
  ( -- * Creating a Request
    ModifyDBInstance (..),
    newModifyDBInstance,

    -- * Request Lenses
    modifyDBInstance_applyImmediately,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_promotionTier,
    modifyDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    ModifyDBInstanceResponse (..),
    newModifyDBInstanceResponse,

    -- * Response Lenses
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to ModifyDBInstance.
--
-- /See:/ 'newModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { -- | Specifies whether the modifications in this request and any pending
    -- modifications are asynchronously applied as soon as possible, regardless
    -- of the @PreferredMaintenanceWindow@ setting for the instance.
    --
    -- If this parameter is set to @false@, changes to the instance are applied
    -- during the next maintenance window. Some parameter changes can cause an
    -- outage and are applied on the next reboot.
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
    -- does not perform minor version upgrades regardless of the value set.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the certificate that needs to be associated with the instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The new compute and memory capacity of the instance; for example,
    -- @db.r5.large@. Not all instance classes are available in all Amazon Web
    -- Services Regions.
    --
    -- If you modify the instance class, an outage occurs during the change.
    -- The change is applied during the next maintenance window, unless
    -- @ApplyImmediately@ is specified as @true@ for this request.
    --
    -- Default: Uses existing setting.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- Instance. For more information, see
    -- <https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Using Amazon Performance Insights>.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The new instance identifier for the instance when renaming an instance.
    -- When you change the instance identifier, an instance reboot occurs
    -- immediately if you set @Apply Immediately@ to @true@. It occurs during
    -- the next maintenance window if you set @Apply Immediately@ to @false@.
    -- This value is stored as a lowercase string.
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
    newDBInstanceIdentifier' :: Prelude.Maybe Prelude.Text,
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
    -- | The weekly time range (in UTC) during which system maintenance can
    -- occur, which might result in an outage. Changing this parameter doesn\'t
    -- result in an outage except in the following situation, and the change is
    -- asynchronously applied as soon as possible. If there are pending actions
    -- that cause a reboot, and the maintenance window is changed to include
    -- the current time, changing this parameter causes a reboot of the
    -- instance. If you are moving this window to the current time, there must
    -- be at least 30 minutes between the current time and end of the window to
    -- ensure that pending changes are applied.
    --
    -- Default: Uses existing setting.
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- Constraints: Must be at least 30 minutes.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Amazon DocumentDB replica
    -- is promoted to the primary instance after a failure of the existing
    -- primary instance.
    --
    -- Default: 1
    --
    -- Valid values: 0-15
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | The instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing @DBInstance@.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyImmediately', 'modifyDBInstance_applyImmediately' - Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless
-- of the @PreferredMaintenanceWindow@ setting for the instance.
--
-- If this parameter is set to @false@, changes to the instance are applied
-- during the next maintenance window. Some parameter changes can cause an
-- outage and are applied on the next reboot.
--
-- Default: @false@
--
-- 'autoMinorVersionUpgrade', 'modifyDBInstance_autoMinorVersionUpgrade' - This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
-- does not perform minor version upgrades regardless of the value set.
--
-- 'cACertificateIdentifier', 'modifyDBInstance_cACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
--
-- 'copyTagsToSnapshot', 'modifyDBInstance_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- 'dbInstanceClass', 'modifyDBInstance_dbInstanceClass' - The new compute and memory capacity of the instance; for example,
-- @db.r5.large@. Not all instance classes are available in all Amazon Web
-- Services Regions.
--
-- If you modify the instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is specified as @true@ for this request.
--
-- Default: Uses existing setting.
--
-- 'enablePerformanceInsights', 'modifyDBInstance_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- Instance. For more information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Using Amazon Performance Insights>.
--
-- 'newDBInstanceIdentifier'', 'modifyDBInstance_newDBInstanceIdentifier' - The new instance identifier for the instance when renaming an instance.
-- When you change the instance identifier, an instance reboot occurs
-- immediately if you set @Apply Immediately@ to @true@. It occurs during
-- the next maintenance window if you set @Apply Immediately@ to @false@.
-- This value is stored as a lowercase string.
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
-- 'performanceInsightsKMSKeyId', 'modifyDBInstance_performanceInsightsKMSKeyId' - The KMS key identifier for encryption of Performance Insights data.
--
-- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
-- for the KMS key.
--
-- If you do not specify a value for PerformanceInsightsKMSKeyId, then
-- Amazon DocumentDB uses your default KMS key. There is a default KMS key
-- for your Amazon Web Services account. Your Amazon Web Services account
-- has a different default KMS key for each Amazon Web Services region.
--
-- 'preferredMaintenanceWindow', 'modifyDBInstance_preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can
-- occur, which might result in an outage. Changing this parameter doesn\'t
-- result in an outage except in the following situation, and the change is
-- asynchronously applied as soon as possible. If there are pending actions
-- that cause a reboot, and the maintenance window is changed to include
-- the current time, changing this parameter causes a reboot of the
-- instance. If you are moving this window to the current time, there must
-- be at least 30 minutes between the current time and end of the window to
-- ensure that pending changes are applied.
--
-- Default: Uses existing setting.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Must be at least 30 minutes.
--
-- 'promotionTier', 'modifyDBInstance_promotionTier' - A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
--
-- Default: 1
--
-- Valid values: 0-15
--
-- 'dbInstanceIdentifier', 'modifyDBInstance_dbInstanceIdentifier' - The instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing @DBInstance@.
newModifyDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  ModifyDBInstance
newModifyDBInstance pDBInstanceIdentifier_ =
  ModifyDBInstance'
    { applyImmediately =
        Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      enablePerformanceInsights = Prelude.Nothing,
      newDBInstanceIdentifier' = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | Specifies whether the modifications in this request and any pending
-- modifications are asynchronously applied as soon as possible, regardless
-- of the @PreferredMaintenanceWindow@ setting for the instance.
--
-- If this parameter is set to @false@, changes to the instance are applied
-- during the next maintenance window. Some parameter changes can cause an
-- outage and are applied on the next reboot.
--
-- Default: @false@
modifyDBInstance_applyImmediately :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_applyImmediately = Lens.lens (\ModifyDBInstance' {applyImmediately} -> applyImmediately) (\s@ModifyDBInstance' {} a -> s {applyImmediately = a} :: ModifyDBInstance)

-- | This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
-- does not perform minor version upgrades regardless of the value set.
modifyDBInstance_autoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_autoMinorVersionUpgrade = Lens.lens (\ModifyDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyDBInstance)

-- | Indicates the certificate that needs to be associated with the instance.
modifyDBInstance_cACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_cACertificateIdentifier = Lens.lens (\ModifyDBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@ModifyDBInstance' {} a -> s {cACertificateIdentifier = a} :: ModifyDBInstance)

-- | A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
modifyDBInstance_copyTagsToSnapshot :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_copyTagsToSnapshot = Lens.lens (\ModifyDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBInstance' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBInstance)

-- | The new compute and memory capacity of the instance; for example,
-- @db.r5.large@. Not all instance classes are available in all Amazon Web
-- Services Regions.
--
-- If you modify the instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is specified as @true@ for this request.
--
-- Default: Uses existing setting.
modifyDBInstance_dbInstanceClass :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbInstanceClass = Lens.lens (\ModifyDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@ModifyDBInstance' {} a -> s {dbInstanceClass = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable Performance Insights for the DB
-- Instance. For more information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/performance-insights.html Using Amazon Performance Insights>.
modifyDBInstance_enablePerformanceInsights :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_enablePerformanceInsights = Lens.lens (\ModifyDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@ModifyDBInstance' {} a -> s {enablePerformanceInsights = a} :: ModifyDBInstance)

-- | The new instance identifier for the instance when renaming an instance.
-- When you change the instance identifier, an instance reboot occurs
-- immediately if you set @Apply Immediately@ to @true@. It occurs during
-- the next maintenance window if you set @Apply Immediately@ to @false@.
-- This value is stored as a lowercase string.
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
modifyDBInstance_newDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_newDBInstanceIdentifier = Lens.lens (\ModifyDBInstance' {newDBInstanceIdentifier'} -> newDBInstanceIdentifier') (\s@ModifyDBInstance' {} a -> s {newDBInstanceIdentifier' = a} :: ModifyDBInstance)

-- | The KMS key identifier for encryption of Performance Insights data.
--
-- The KMS key identifier is the key ARN, key ID, alias ARN, or alias name
-- for the KMS key.
--
-- If you do not specify a value for PerformanceInsightsKMSKeyId, then
-- Amazon DocumentDB uses your default KMS key. There is a default KMS key
-- for your Amazon Web Services account. Your Amazon Web Services account
-- has a different default KMS key for each Amazon Web Services region.
modifyDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_performanceInsightsKMSKeyId = Lens.lens (\ModifyDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@ModifyDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: ModifyDBInstance)

-- | The weekly time range (in UTC) during which system maintenance can
-- occur, which might result in an outage. Changing this parameter doesn\'t
-- result in an outage except in the following situation, and the change is
-- asynchronously applied as soon as possible. If there are pending actions
-- that cause a reboot, and the maintenance window is changed to include
-- the current time, changing this parameter causes a reboot of the
-- instance. If you are moving this window to the current time, there must
-- be at least 30 minutes between the current time and end of the window to
-- ensure that pending changes are applied.
--
-- Default: Uses existing setting.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Must be at least 30 minutes.
modifyDBInstance_preferredMaintenanceWindow :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_preferredMaintenanceWindow = Lens.lens (\ModifyDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBInstance)

-- | A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
--
-- Default: 1
--
-- Valid values: 0-15
modifyDBInstance_promotionTier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_promotionTier = Lens.lens (\ModifyDBInstance' {promotionTier} -> promotionTier) (\s@ModifyDBInstance' {} a -> s {promotionTier = a} :: ModifyDBInstance)

-- | The instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing @DBInstance@.
modifyDBInstance_dbInstanceIdentifier :: Lens.Lens' ModifyDBInstance Prelude.Text
modifyDBInstance_dbInstanceIdentifier = Lens.lens (\ModifyDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@ModifyDBInstance' {} a -> s {dbInstanceIdentifier = a} :: ModifyDBInstance)

instance Core.AWSRequest ModifyDBInstance where
  type
    AWSResponse ModifyDBInstance =
      ModifyDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBInstanceResult"
      ( \s h x ->
          ModifyDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBInstance where
  hashWithSalt _salt ModifyDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` newDBInstanceIdentifier'
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData ModifyDBInstance where
  rnf ModifyDBInstance' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf enablePerformanceInsights
      `Prelude.seq` Prelude.rnf newDBInstanceIdentifier'
      `Prelude.seq` Prelude.rnf performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf promotionTier
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders ModifyDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBInstance where
  toQuery ModifyDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ApplyImmediately" Data.=: applyImmediately,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "CACertificateIdentifier"
          Data.=: cACertificateIdentifier,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "NewDBInstanceIdentifier"
          Data.=: newDBInstanceIdentifier',
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PromotionTier" Data.=: promotionTier,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newModifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'modifyDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'modifyDBInstanceResponse_httpStatus' - The response's http status code.
newModifyDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBInstanceResponse
newModifyDBInstanceResponse pHttpStatus_ =
  ModifyDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBInstanceResponse_dbInstance :: Lens.Lens' ModifyDBInstanceResponse (Prelude.Maybe DBInstance)
modifyDBInstanceResponse_dbInstance = Lens.lens (\ModifyDBInstanceResponse' {dbInstance} -> dbInstance) (\s@ModifyDBInstanceResponse' {} a -> s {dbInstance = a} :: ModifyDBInstanceResponse)

-- | The response's http status code.
modifyDBInstanceResponse_httpStatus :: Lens.Lens' ModifyDBInstanceResponse Prelude.Int
modifyDBInstanceResponse_httpStatus = Lens.lens (\ModifyDBInstanceResponse' {httpStatus} -> httpStatus) (\s@ModifyDBInstanceResponse' {} a -> s {httpStatus = a} :: ModifyDBInstanceResponse)

instance Prelude.NFData ModifyDBInstanceResponse where
  rnf ModifyDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
