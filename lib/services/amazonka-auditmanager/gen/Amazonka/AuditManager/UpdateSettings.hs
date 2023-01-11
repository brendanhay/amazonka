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
-- Module      : Amazonka.AuditManager.UpdateSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates Audit Manager settings for the current user account.
module Amazonka.AuditManager.UpdateSettings
  ( -- * Creating a Request
    UpdateSettings (..),
    newUpdateSettings,

    -- * Request Lenses
    updateSettings_defaultAssessmentReportsDestination,
    updateSettings_defaultProcessOwners,
    updateSettings_deregistrationPolicy,
    updateSettings_evidenceFinderEnabled,
    updateSettings_kmsKey,
    updateSettings_snsTopic,

    -- * Destructuring the Response
    UpdateSettingsResponse (..),
    newUpdateSettingsResponse,

    -- * Response Lenses
    updateSettingsResponse_settings,
    updateSettingsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSettings' smart constructor.
data UpdateSettings = UpdateSettings'
  { -- | The default storage destination for assessment reports.
    defaultAssessmentReportsDestination :: Prelude.Maybe AssessmentReportsDestination,
    -- | A list of the default audit owners.
    defaultProcessOwners :: Prelude.Maybe [Role],
    -- | The deregistration policy for your Audit Manager data. You can use this
    -- attribute to determine how your data is handled when you deregister
    -- Audit Manager.
    deregistrationPolicy :: Prelude.Maybe DeregistrationPolicy,
    -- | Specifies whether the evidence finder feature is enabled. Change this
    -- attribute to enable or disable evidence finder.
    --
    -- When you use this attribute to disable evidence finder, Audit Manager
    -- deletes the event data store that’s used to query your evidence data. As
    -- a result, you can’t re-enable evidence finder and use the feature again.
    -- Your only alternative is to
    -- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeregisterAccount.html deregister>
    -- and then
    -- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_RegisterAccount.html re-register>
    -- Audit Manager.
    evidenceFinderEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key details.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Notification Service (Amazon SNS) topic that Audit
    -- Manager sends notifications to.
    snsTopic :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAssessmentReportsDestination', 'updateSettings_defaultAssessmentReportsDestination' - The default storage destination for assessment reports.
--
-- 'defaultProcessOwners', 'updateSettings_defaultProcessOwners' - A list of the default audit owners.
--
-- 'deregistrationPolicy', 'updateSettings_deregistrationPolicy' - The deregistration policy for your Audit Manager data. You can use this
-- attribute to determine how your data is handled when you deregister
-- Audit Manager.
--
-- 'evidenceFinderEnabled', 'updateSettings_evidenceFinderEnabled' - Specifies whether the evidence finder feature is enabled. Change this
-- attribute to enable or disable evidence finder.
--
-- When you use this attribute to disable evidence finder, Audit Manager
-- deletes the event data store that’s used to query your evidence data. As
-- a result, you can’t re-enable evidence finder and use the feature again.
-- Your only alternative is to
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeregisterAccount.html deregister>
-- and then
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_RegisterAccount.html re-register>
-- Audit Manager.
--
-- 'kmsKey', 'updateSettings_kmsKey' - The KMS key details.
--
-- 'snsTopic', 'updateSettings_snsTopic' - The Amazon Simple Notification Service (Amazon SNS) topic that Audit
-- Manager sends notifications to.
newUpdateSettings ::
  UpdateSettings
newUpdateSettings =
  UpdateSettings'
    { defaultAssessmentReportsDestination =
        Prelude.Nothing,
      defaultProcessOwners = Prelude.Nothing,
      deregistrationPolicy = Prelude.Nothing,
      evidenceFinderEnabled = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      snsTopic = Prelude.Nothing
    }

-- | The default storage destination for assessment reports.
updateSettings_defaultAssessmentReportsDestination :: Lens.Lens' UpdateSettings (Prelude.Maybe AssessmentReportsDestination)
updateSettings_defaultAssessmentReportsDestination = Lens.lens (\UpdateSettings' {defaultAssessmentReportsDestination} -> defaultAssessmentReportsDestination) (\s@UpdateSettings' {} a -> s {defaultAssessmentReportsDestination = a} :: UpdateSettings)

-- | A list of the default audit owners.
updateSettings_defaultProcessOwners :: Lens.Lens' UpdateSettings (Prelude.Maybe [Role])
updateSettings_defaultProcessOwners = Lens.lens (\UpdateSettings' {defaultProcessOwners} -> defaultProcessOwners) (\s@UpdateSettings' {} a -> s {defaultProcessOwners = a} :: UpdateSettings) Prelude.. Lens.mapping Lens.coerced

-- | The deregistration policy for your Audit Manager data. You can use this
-- attribute to determine how your data is handled when you deregister
-- Audit Manager.
updateSettings_deregistrationPolicy :: Lens.Lens' UpdateSettings (Prelude.Maybe DeregistrationPolicy)
updateSettings_deregistrationPolicy = Lens.lens (\UpdateSettings' {deregistrationPolicy} -> deregistrationPolicy) (\s@UpdateSettings' {} a -> s {deregistrationPolicy = a} :: UpdateSettings)

-- | Specifies whether the evidence finder feature is enabled. Change this
-- attribute to enable or disable evidence finder.
--
-- When you use this attribute to disable evidence finder, Audit Manager
-- deletes the event data store that’s used to query your evidence data. As
-- a result, you can’t re-enable evidence finder and use the feature again.
-- Your only alternative is to
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_DeregisterAccount.html deregister>
-- and then
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_RegisterAccount.html re-register>
-- Audit Manager.
updateSettings_evidenceFinderEnabled :: Lens.Lens' UpdateSettings (Prelude.Maybe Prelude.Bool)
updateSettings_evidenceFinderEnabled = Lens.lens (\UpdateSettings' {evidenceFinderEnabled} -> evidenceFinderEnabled) (\s@UpdateSettings' {} a -> s {evidenceFinderEnabled = a} :: UpdateSettings)

-- | The KMS key details.
updateSettings_kmsKey :: Lens.Lens' UpdateSettings (Prelude.Maybe Prelude.Text)
updateSettings_kmsKey = Lens.lens (\UpdateSettings' {kmsKey} -> kmsKey) (\s@UpdateSettings' {} a -> s {kmsKey = a} :: UpdateSettings)

-- | The Amazon Simple Notification Service (Amazon SNS) topic that Audit
-- Manager sends notifications to.
updateSettings_snsTopic :: Lens.Lens' UpdateSettings (Prelude.Maybe Prelude.Text)
updateSettings_snsTopic = Lens.lens (\UpdateSettings' {snsTopic} -> snsTopic) (\s@UpdateSettings' {} a -> s {snsTopic = a} :: UpdateSettings)

instance Core.AWSRequest UpdateSettings where
  type
    AWSResponse UpdateSettings =
      UpdateSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSettingsResponse'
            Prelude.<$> (x Data..?> "settings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSettings where
  hashWithSalt _salt UpdateSettings' {..} =
    _salt
      `Prelude.hashWithSalt` defaultAssessmentReportsDestination
      `Prelude.hashWithSalt` defaultProcessOwners
      `Prelude.hashWithSalt` deregistrationPolicy
      `Prelude.hashWithSalt` evidenceFinderEnabled
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` snsTopic

instance Prelude.NFData UpdateSettings where
  rnf UpdateSettings' {..} =
    Prelude.rnf defaultAssessmentReportsDestination
      `Prelude.seq` Prelude.rnf defaultProcessOwners
      `Prelude.seq` Prelude.rnf deregistrationPolicy
      `Prelude.seq` Prelude.rnf evidenceFinderEnabled
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf snsTopic

instance Data.ToHeaders UpdateSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSettings where
  toJSON UpdateSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultAssessmentReportsDestination" Data..=)
              Prelude.<$> defaultAssessmentReportsDestination,
            ("defaultProcessOwners" Data..=)
              Prelude.<$> defaultProcessOwners,
            ("deregistrationPolicy" Data..=)
              Prelude.<$> deregistrationPolicy,
            ("evidenceFinderEnabled" Data..=)
              Prelude.<$> evidenceFinderEnabled,
            ("kmsKey" Data..=) Prelude.<$> kmsKey,
            ("snsTopic" Data..=) Prelude.<$> snsTopic
          ]
      )

instance Data.ToPath UpdateSettings where
  toPath = Prelude.const "/settings"

instance Data.ToQuery UpdateSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSettingsResponse' smart constructor.
data UpdateSettingsResponse = UpdateSettingsResponse'
  { -- | The current list of settings.
    settings :: Prelude.Maybe Settings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'updateSettingsResponse_settings' - The current list of settings.
--
-- 'httpStatus', 'updateSettingsResponse_httpStatus' - The response's http status code.
newUpdateSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSettingsResponse
newUpdateSettingsResponse pHttpStatus_ =
  UpdateSettingsResponse'
    { settings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current list of settings.
updateSettingsResponse_settings :: Lens.Lens' UpdateSettingsResponse (Prelude.Maybe Settings)
updateSettingsResponse_settings = Lens.lens (\UpdateSettingsResponse' {settings} -> settings) (\s@UpdateSettingsResponse' {} a -> s {settings = a} :: UpdateSettingsResponse)

-- | The response's http status code.
updateSettingsResponse_httpStatus :: Lens.Lens' UpdateSettingsResponse Prelude.Int
updateSettingsResponse_httpStatus = Lens.lens (\UpdateSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateSettingsResponse' {} a -> s {httpStatus = a} :: UpdateSettingsResponse)

instance Prelude.NFData UpdateSettingsResponse where
  rnf UpdateSettingsResponse' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf httpStatus
