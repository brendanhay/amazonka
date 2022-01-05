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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_promotionTier,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_applyImmediately,
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
import Amazonka.DocumentDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to ModifyDBInstance.
--
-- /See:/ 'newModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { -- | This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
    -- does not perform minor version upgrades regardless of the value set.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
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
    -- | The new compute and memory capacity of the instance; for example,
    -- @db.r5.large@. Not all instance classes are available in all Regions.
    --
    -- If you modify the instance class, an outage occurs during the change.
    -- The change is applied during the next maintenance window, unless
    -- @ApplyImmediately@ is specified as @true@ for this request.
    --
    -- Default: Uses existing setting.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Amazon DocumentDB replica
    -- is promoted to the primary instance after a failure of the existing
    -- primary instance.
    --
    -- Default: 1
    --
    -- Valid values: 0-15
    promotionTier :: Prelude.Maybe Prelude.Int,
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
    -- | Indicates the certificate that needs to be associated with the instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the modifications in this request and any pending
    -- modifications are asynchronously applied as soon as possible, regardless
    -- of the @PreferredMaintenanceWindow@ setting for the instance.
    --
    -- If this parameter is set to @false@, changes to the instance are applied
    -- during the next maintenance window. Some parameter changes can cause an
    -- outage and are applied on the next reboot.
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
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
-- 'autoMinorVersionUpgrade', 'modifyDBInstance_autoMinorVersionUpgrade' - This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
-- does not perform minor version upgrades regardless of the value set.
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
-- 'dbInstanceClass', 'modifyDBInstance_dbInstanceClass' - The new compute and memory capacity of the instance; for example,
-- @db.r5.large@. Not all instance classes are available in all Regions.
--
-- If you modify the instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is specified as @true@ for this request.
--
-- Default: Uses existing setting.
--
-- 'promotionTier', 'modifyDBInstance_promotionTier' - A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
--
-- Default: 1
--
-- Valid values: 0-15
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
-- 'cACertificateIdentifier', 'modifyDBInstance_cACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
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
    { autoMinorVersionUpgrade =
        Prelude.Nothing,
      newDBInstanceIdentifier' = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | This parameter does not apply to Amazon DocumentDB. Amazon DocumentDB
-- does not perform minor version upgrades regardless of the value set.
modifyDBInstance_autoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_autoMinorVersionUpgrade = Lens.lens (\ModifyDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyDBInstance)

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

-- | The new compute and memory capacity of the instance; for example,
-- @db.r5.large@. Not all instance classes are available in all Regions.
--
-- If you modify the instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is specified as @true@ for this request.
--
-- Default: Uses existing setting.
modifyDBInstance_dbInstanceClass :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbInstanceClass = Lens.lens (\ModifyDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@ModifyDBInstance' {} a -> s {dbInstanceClass = a} :: ModifyDBInstance)

-- | A value that specifies the order in which an Amazon DocumentDB replica
-- is promoted to the primary instance after a failure of the existing
-- primary instance.
--
-- Default: 1
--
-- Valid values: 0-15
modifyDBInstance_promotionTier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_promotionTier = Lens.lens (\ModifyDBInstance' {promotionTier} -> promotionTier) (\s@ModifyDBInstance' {} a -> s {promotionTier = a} :: ModifyDBInstance)

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

-- | Indicates the certificate that needs to be associated with the instance.
modifyDBInstance_cACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_cACertificateIdentifier = Lens.lens (\ModifyDBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@ModifyDBInstance' {} a -> s {cACertificateIdentifier = a} :: ModifyDBInstance)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBInstanceResult"
      ( \s h x ->
          ModifyDBInstanceResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBInstance where
  hashWithSalt _salt ModifyDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` newDBInstanceIdentifier'
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData ModifyDBInstance where
  rnf ModifyDBInstance' {..} =
    Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf newDBInstanceIdentifier'
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf promotionTier
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier

instance Core.ToHeaders ModifyDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBInstance where
  toQuery ModifyDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBInstance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "NewDBInstanceIdentifier"
          Core.=: newDBInstanceIdentifier',
        "DBInstanceClass" Core.=: dbInstanceClass,
        "PromotionTier" Core.=: promotionTier,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "CACertificateIdentifier"
          Core.=: cACertificateIdentifier,
        "ApplyImmediately" Core.=: applyImmediately,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
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
