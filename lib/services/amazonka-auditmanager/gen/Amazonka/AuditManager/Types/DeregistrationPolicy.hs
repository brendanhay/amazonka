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
-- Module      : Amazonka.AuditManager.Types.DeregistrationPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.DeregistrationPolicy where

import Amazonka.AuditManager.Types.DeleteResources
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The deregistration policy for the data that\'s stored in Audit Manager.
-- You can use this attribute to determine how your data is handled when
-- you
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeregisterAccount.html deregister Audit Manager>.
--
-- By default, Audit Manager retains evidence data for two years from the
-- time of its creation. Other Audit Manager resources (including
-- assessments, custom controls, and custom frameworks) remain in Audit
-- Manager indefinitely, and are available if you
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_RegisterAccount.html re-register Audit Manager>
-- in the future. For more information about data retention, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/data-protection.html Data Protection>
-- in the /Audit Manager User Guide/.
--
-- If you choose to delete all data, this action permanently deletes all
-- evidence data in your account within seven days. It also deletes all of
-- the Audit Manager resources that you created, including assessments,
-- custom controls, and custom frameworks. Your data will not be available
-- if you re-register Audit Manager in the future.
--
-- /See:/ 'newDeregistrationPolicy' smart constructor.
data DeregistrationPolicy = DeregistrationPolicy'
  { -- | Specifies which Audit Manager data will be deleted when you deregister
    -- Audit Manager.
    --
    -- -   If you set the value to @ALL@, all of your data is deleted within
    --     seven days of deregistration.
    --
    -- -   If you set the value to @DEFAULT@, none of your data is deleted at
    --     the time of deregistration. However, keep in mind that the Audit
    --     Manager data retention policy still applies. As a result, any
    --     evidence data will be deleted two years after its creation date.
    --     Your other Audit Manager resources will continue to exist
    --     indefinitely.
    deleteResources :: Prelude.Maybe DeleteResources
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregistrationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteResources', 'deregistrationPolicy_deleteResources' - Specifies which Audit Manager data will be deleted when you deregister
-- Audit Manager.
--
-- -   If you set the value to @ALL@, all of your data is deleted within
--     seven days of deregistration.
--
-- -   If you set the value to @DEFAULT@, none of your data is deleted at
--     the time of deregistration. However, keep in mind that the Audit
--     Manager data retention policy still applies. As a result, any
--     evidence data will be deleted two years after its creation date.
--     Your other Audit Manager resources will continue to exist
--     indefinitely.
newDeregistrationPolicy ::
  DeregistrationPolicy
newDeregistrationPolicy =
  DeregistrationPolicy'
    { deleteResources =
        Prelude.Nothing
    }

-- | Specifies which Audit Manager data will be deleted when you deregister
-- Audit Manager.
--
-- -   If you set the value to @ALL@, all of your data is deleted within
--     seven days of deregistration.
--
-- -   If you set the value to @DEFAULT@, none of your data is deleted at
--     the time of deregistration. However, keep in mind that the Audit
--     Manager data retention policy still applies. As a result, any
--     evidence data will be deleted two years after its creation date.
--     Your other Audit Manager resources will continue to exist
--     indefinitely.
deregistrationPolicy_deleteResources :: Lens.Lens' DeregistrationPolicy (Prelude.Maybe DeleteResources)
deregistrationPolicy_deleteResources = Lens.lens (\DeregistrationPolicy' {deleteResources} -> deleteResources) (\s@DeregistrationPolicy' {} a -> s {deleteResources = a} :: DeregistrationPolicy)

instance Data.FromJSON DeregistrationPolicy where
  parseJSON =
    Data.withObject
      "DeregistrationPolicy"
      ( \x ->
          DeregistrationPolicy'
            Prelude.<$> (x Data..:? "deleteResources")
      )

instance Prelude.Hashable DeregistrationPolicy where
  hashWithSalt _salt DeregistrationPolicy' {..} =
    _salt `Prelude.hashWithSalt` deleteResources

instance Prelude.NFData DeregistrationPolicy where
  rnf DeregistrationPolicy' {..} =
    Prelude.rnf deleteResources

instance Data.ToJSON DeregistrationPolicy where
  toJSON DeregistrationPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deleteResources" Data..=)
              Prelude.<$> deleteResources
          ]
      )
