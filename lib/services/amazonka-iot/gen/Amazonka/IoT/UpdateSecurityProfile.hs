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
-- Module      : Amazonka.IoT.UpdateSecurityProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender security profile.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateSecurityProfile>
-- action.
module Amazonka.IoT.UpdateSecurityProfile
  ( -- * Creating a Request
    UpdateSecurityProfile (..),
    newUpdateSecurityProfile,

    -- * Request Lenses
    updateSecurityProfile_deleteBehaviors,
    updateSecurityProfile_alertTargets,
    updateSecurityProfile_securityProfileDescription,
    updateSecurityProfile_deleteAlertTargets,
    updateSecurityProfile_additionalMetricsToRetainV2,
    updateSecurityProfile_deleteAdditionalMetricsToRetain,
    updateSecurityProfile_expectedVersion,
    updateSecurityProfile_additionalMetricsToRetain,
    updateSecurityProfile_behaviors,
    updateSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    UpdateSecurityProfileResponse (..),
    newUpdateSecurityProfileResponse,

    -- * Response Lenses
    updateSecurityProfileResponse_lastModifiedDate,
    updateSecurityProfileResponse_alertTargets,
    updateSecurityProfileResponse_creationDate,
    updateSecurityProfileResponse_securityProfileDescription,
    updateSecurityProfileResponse_additionalMetricsToRetainV2,
    updateSecurityProfileResponse_securityProfileName,
    updateSecurityProfileResponse_securityProfileArn,
    updateSecurityProfileResponse_additionalMetricsToRetain,
    updateSecurityProfileResponse_version,
    updateSecurityProfileResponse_behaviors,
    updateSecurityProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSecurityProfile' smart constructor.
data UpdateSecurityProfile = UpdateSecurityProfile'
  { -- | If true, delete all @behaviors@ defined for this security profile. If
    -- any @behaviors@ are defined in the current invocation, an exception
    -- occurs.
    deleteBehaviors :: Prelude.Maybe Prelude.Bool,
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget),
    -- | A description of the security profile.
    securityProfileDescription :: Prelude.Maybe Prelude.Text,
    -- | If true, delete all @alertTargets@ defined for this security profile. If
    -- any @alertTargets@ are defined in the current invocation, an exception
    -- occurs.
    deleteAlertTargets :: Prelude.Maybe Prelude.Bool,
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s behaviors, but it is also
    -- retained for any metric specified here. Can be used with custom metrics;
    -- cannot be used with dimensions.
    additionalMetricsToRetainV2 :: Prelude.Maybe [MetricToRetain],
    -- | If true, delete all @additionalMetricsToRetain@ defined for this
    -- security profile. If any @additionalMetricsToRetain@ are defined in the
    -- current invocation, an exception occurs.
    deleteAdditionalMetricsToRetain :: Prelude.Maybe Prelude.Bool,
    -- | The expected version of the security profile. A new version is generated
    -- whenever the security profile is updated. If you specify a value that is
    -- different from the actual version, a @VersionConflictException@ is
    -- thrown.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | /Please use UpdateSecurityProfileRequest$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here. Can be used with custom
    -- metrics; cannot be used with dimensions.
    additionalMetricsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Prelude.Maybe [Behavior],
    -- | The name of the security profile you want to update.
    securityProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteBehaviors', 'updateSecurityProfile_deleteBehaviors' - If true, delete all @behaviors@ defined for this security profile. If
-- any @behaviors@ are defined in the current invocation, an exception
-- occurs.
--
-- 'alertTargets', 'updateSecurityProfile_alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- 'securityProfileDescription', 'updateSecurityProfile_securityProfileDescription' - A description of the security profile.
--
-- 'deleteAlertTargets', 'updateSecurityProfile_deleteAlertTargets' - If true, delete all @alertTargets@ defined for this security profile. If
-- any @alertTargets@ are defined in the current invocation, an exception
-- occurs.
--
-- 'additionalMetricsToRetainV2', 'updateSecurityProfile_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
--
-- 'deleteAdditionalMetricsToRetain', 'updateSecurityProfile_deleteAdditionalMetricsToRetain' - If true, delete all @additionalMetricsToRetain@ defined for this
-- security profile. If any @additionalMetricsToRetain@ are defined in the
-- current invocation, an exception occurs.
--
-- 'expectedVersion', 'updateSecurityProfile_expectedVersion' - The expected version of the security profile. A new version is generated
-- whenever the security profile is updated. If you specify a value that is
-- different from the actual version, a @VersionConflictException@ is
-- thrown.
--
-- 'additionalMetricsToRetain', 'updateSecurityProfile_additionalMetricsToRetain' - /Please use UpdateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
--
-- 'behaviors', 'updateSecurityProfile_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'securityProfileName', 'updateSecurityProfile_securityProfileName' - The name of the security profile you want to update.
newUpdateSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  UpdateSecurityProfile
newUpdateSecurityProfile pSecurityProfileName_ =
  UpdateSecurityProfile'
    { deleteBehaviors =
        Prelude.Nothing,
      alertTargets = Prelude.Nothing,
      securityProfileDescription = Prelude.Nothing,
      deleteAlertTargets = Prelude.Nothing,
      additionalMetricsToRetainV2 = Prelude.Nothing,
      deleteAdditionalMetricsToRetain = Prelude.Nothing,
      expectedVersion = Prelude.Nothing,
      additionalMetricsToRetain = Prelude.Nothing,
      behaviors = Prelude.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | If true, delete all @behaviors@ defined for this security profile. If
-- any @behaviors@ are defined in the current invocation, an exception
-- occurs.
updateSecurityProfile_deleteBehaviors :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Bool)
updateSecurityProfile_deleteBehaviors = Lens.lens (\UpdateSecurityProfile' {deleteBehaviors} -> deleteBehaviors) (\s@UpdateSecurityProfile' {} a -> s {deleteBehaviors = a} :: UpdateSecurityProfile)

-- | Where the alerts are sent. (Alerts are always sent to the console.)
updateSecurityProfile_alertTargets :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget))
updateSecurityProfile_alertTargets = Lens.lens (\UpdateSecurityProfile' {alertTargets} -> alertTargets) (\s@UpdateSecurityProfile' {} a -> s {alertTargets = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | A description of the security profile.
updateSecurityProfile_securityProfileDescription :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Text)
updateSecurityProfile_securityProfileDescription = Lens.lens (\UpdateSecurityProfile' {securityProfileDescription} -> securityProfileDescription) (\s@UpdateSecurityProfile' {} a -> s {securityProfileDescription = a} :: UpdateSecurityProfile)

-- | If true, delete all @alertTargets@ defined for this security profile. If
-- any @alertTargets@ are defined in the current invocation, an exception
-- occurs.
updateSecurityProfile_deleteAlertTargets :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Bool)
updateSecurityProfile_deleteAlertTargets = Lens.lens (\UpdateSecurityProfile' {deleteAlertTargets} -> deleteAlertTargets) (\s@UpdateSecurityProfile' {} a -> s {deleteAlertTargets = a} :: UpdateSecurityProfile)

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
updateSecurityProfile_additionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [MetricToRetain])
updateSecurityProfile_additionalMetricsToRetainV2 = Lens.lens (\UpdateSecurityProfile' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@UpdateSecurityProfile' {} a -> s {additionalMetricsToRetainV2 = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | If true, delete all @additionalMetricsToRetain@ defined for this
-- security profile. If any @additionalMetricsToRetain@ are defined in the
-- current invocation, an exception occurs.
updateSecurityProfile_deleteAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Bool)
updateSecurityProfile_deleteAdditionalMetricsToRetain = Lens.lens (\UpdateSecurityProfile' {deleteAdditionalMetricsToRetain} -> deleteAdditionalMetricsToRetain) (\s@UpdateSecurityProfile' {} a -> s {deleteAdditionalMetricsToRetain = a} :: UpdateSecurityProfile)

-- | The expected version of the security profile. A new version is generated
-- whenever the security profile is updated. If you specify a value that is
-- different from the actual version, a @VersionConflictException@ is
-- thrown.
updateSecurityProfile_expectedVersion :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Integer)
updateSecurityProfile_expectedVersion = Lens.lens (\UpdateSecurityProfile' {expectedVersion} -> expectedVersion) (\s@UpdateSecurityProfile' {} a -> s {expectedVersion = a} :: UpdateSecurityProfile)

-- | /Please use UpdateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
updateSecurityProfile_additionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [Prelude.Text])
updateSecurityProfile_additionalMetricsToRetain = Lens.lens (\UpdateSecurityProfile' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@UpdateSecurityProfile' {} a -> s {additionalMetricsToRetain = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
updateSecurityProfile_behaviors :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [Behavior])
updateSecurityProfile_behaviors = Lens.lens (\UpdateSecurityProfile' {behaviors} -> behaviors) (\s@UpdateSecurityProfile' {} a -> s {behaviors = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the security profile you want to update.
updateSecurityProfile_securityProfileName :: Lens.Lens' UpdateSecurityProfile Prelude.Text
updateSecurityProfile_securityProfileName = Lens.lens (\UpdateSecurityProfile' {securityProfileName} -> securityProfileName) (\s@UpdateSecurityProfile' {} a -> s {securityProfileName = a} :: UpdateSecurityProfile)

instance Core.AWSRequest UpdateSecurityProfile where
  type
    AWSResponse UpdateSecurityProfile =
      UpdateSecurityProfileResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecurityProfileResponse'
            Prelude.<$> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "alertTargets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "securityProfileDescription")
            Prelude.<*> ( x Data..?> "additionalMetricsToRetainV2"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "securityProfileName")
            Prelude.<*> (x Data..?> "securityProfileArn")
            Prelude.<*> ( x Data..?> "additionalMetricsToRetain"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (x Data..?> "behaviors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecurityProfile where
  hashWithSalt _salt UpdateSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` deleteBehaviors
      `Prelude.hashWithSalt` alertTargets
      `Prelude.hashWithSalt` securityProfileDescription
      `Prelude.hashWithSalt` deleteAlertTargets
      `Prelude.hashWithSalt` additionalMetricsToRetainV2
      `Prelude.hashWithSalt` deleteAdditionalMetricsToRetain
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` additionalMetricsToRetain
      `Prelude.hashWithSalt` behaviors
      `Prelude.hashWithSalt` securityProfileName

instance Prelude.NFData UpdateSecurityProfile where
  rnf UpdateSecurityProfile' {..} =
    Prelude.rnf deleteBehaviors
      `Prelude.seq` Prelude.rnf alertTargets
      `Prelude.seq` Prelude.rnf securityProfileDescription
      `Prelude.seq` Prelude.rnf deleteAlertTargets
      `Prelude.seq` Prelude.rnf additionalMetricsToRetainV2
      `Prelude.seq` Prelude.rnf deleteAdditionalMetricsToRetain
      `Prelude.seq` Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf additionalMetricsToRetain
      `Prelude.seq` Prelude.rnf behaviors
      `Prelude.seq` Prelude.rnf securityProfileName

instance Data.ToHeaders UpdateSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateSecurityProfile where
  toJSON UpdateSecurityProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deleteBehaviors" Data..=)
              Prelude.<$> deleteBehaviors,
            ("alertTargets" Data..=) Prelude.<$> alertTargets,
            ("securityProfileDescription" Data..=)
              Prelude.<$> securityProfileDescription,
            ("deleteAlertTargets" Data..=)
              Prelude.<$> deleteAlertTargets,
            ("additionalMetricsToRetainV2" Data..=)
              Prelude.<$> additionalMetricsToRetainV2,
            ("deleteAdditionalMetricsToRetain" Data..=)
              Prelude.<$> deleteAdditionalMetricsToRetain,
            ("additionalMetricsToRetain" Data..=)
              Prelude.<$> additionalMetricsToRetain,
            ("behaviors" Data..=) Prelude.<$> behaviors
          ]
      )

instance Data.ToPath UpdateSecurityProfile where
  toPath UpdateSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS securityProfileName
      ]

instance Data.ToQuery UpdateSecurityProfile where
  toQuery UpdateSecurityProfile' {..} =
    Prelude.mconcat
      ["expectedVersion" Data.=: expectedVersion]

-- | /See:/ 'newUpdateSecurityProfileResponse' smart constructor.
data UpdateSecurityProfileResponse = UpdateSecurityProfileResponse'
  { -- | The time the security profile was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget),
    -- | The time the security profile was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the security profile.
    securityProfileDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s behaviors, but it is also
    -- retained for any metric specified here. Can be used with custom metrics;
    -- cannot be used with dimensions.
    additionalMetricsToRetainV2 :: Prelude.Maybe [MetricToRetain],
    -- | The name of the security profile that was updated.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the security profile that was updated.
    securityProfileArn :: Prelude.Maybe Prelude.Text,
    -- | /Please use UpdateSecurityProfileResponse$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the security profile\'s @behaviors@, but
    -- it is also retained for any metric specified here.
    additionalMetricsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | The updated version of the security profile.
    version :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Prelude.Maybe [Behavior],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'updateSecurityProfileResponse_lastModifiedDate' - The time the security profile was last modified.
--
-- 'alertTargets', 'updateSecurityProfileResponse_alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- 'creationDate', 'updateSecurityProfileResponse_creationDate' - The time the security profile was created.
--
-- 'securityProfileDescription', 'updateSecurityProfileResponse_securityProfileDescription' - The description of the security profile.
--
-- 'additionalMetricsToRetainV2', 'updateSecurityProfileResponse_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
--
-- 'securityProfileName', 'updateSecurityProfileResponse_securityProfileName' - The name of the security profile that was updated.
--
-- 'securityProfileArn', 'updateSecurityProfileResponse_securityProfileArn' - The ARN of the security profile that was updated.
--
-- 'additionalMetricsToRetain', 'updateSecurityProfileResponse_additionalMetricsToRetain' - /Please use UpdateSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the security profile\'s @behaviors@, but
-- it is also retained for any metric specified here.
--
-- 'version', 'updateSecurityProfileResponse_version' - The updated version of the security profile.
--
-- 'behaviors', 'updateSecurityProfileResponse_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'httpStatus', 'updateSecurityProfileResponse_httpStatus' - The response's http status code.
newUpdateSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecurityProfileResponse
newUpdateSecurityProfileResponse pHttpStatus_ =
  UpdateSecurityProfileResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      alertTargets = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      securityProfileDescription = Prelude.Nothing,
      additionalMetricsToRetainV2 =
        Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      securityProfileArn = Prelude.Nothing,
      additionalMetricsToRetain = Prelude.Nothing,
      version = Prelude.Nothing,
      behaviors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the security profile was last modified.
updateSecurityProfileResponse_lastModifiedDate :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateSecurityProfileResponse_lastModifiedDate = Lens.lens (\UpdateSecurityProfileResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateSecurityProfileResponse' {} a -> s {lastModifiedDate = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Data._Time

-- | Where the alerts are sent. (Alerts are always sent to the console.)
updateSecurityProfileResponse_alertTargets :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget))
updateSecurityProfileResponse_alertTargets = Lens.lens (\UpdateSecurityProfileResponse' {alertTargets} -> alertTargets) (\s@UpdateSecurityProfileResponse' {} a -> s {alertTargets = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time the security profile was created.
updateSecurityProfileResponse_creationDate :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateSecurityProfileResponse_creationDate = Lens.lens (\UpdateSecurityProfileResponse' {creationDate} -> creationDate) (\s@UpdateSecurityProfileResponse' {} a -> s {creationDate = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the security profile.
updateSecurityProfileResponse_securityProfileDescription :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
updateSecurityProfileResponse_securityProfileDescription = Lens.lens (\UpdateSecurityProfileResponse' {securityProfileDescription} -> securityProfileDescription) (\s@UpdateSecurityProfileResponse' {} a -> s {securityProfileDescription = a} :: UpdateSecurityProfileResponse)

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
updateSecurityProfileResponse_additionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe [MetricToRetain])
updateSecurityProfileResponse_additionalMetricsToRetainV2 = Lens.lens (\UpdateSecurityProfileResponse' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@UpdateSecurityProfileResponse' {} a -> s {additionalMetricsToRetainV2 = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the security profile that was updated.
updateSecurityProfileResponse_securityProfileName :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
updateSecurityProfileResponse_securityProfileName = Lens.lens (\UpdateSecurityProfileResponse' {securityProfileName} -> securityProfileName) (\s@UpdateSecurityProfileResponse' {} a -> s {securityProfileName = a} :: UpdateSecurityProfileResponse)

-- | The ARN of the security profile that was updated.
updateSecurityProfileResponse_securityProfileArn :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
updateSecurityProfileResponse_securityProfileArn = Lens.lens (\UpdateSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@UpdateSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: UpdateSecurityProfileResponse)

-- | /Please use UpdateSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the security profile\'s @behaviors@, but
-- it is also retained for any metric specified here.
updateSecurityProfileResponse_additionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe [Prelude.Text])
updateSecurityProfileResponse_additionalMetricsToRetain = Lens.lens (\UpdateSecurityProfileResponse' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@UpdateSecurityProfileResponse' {} a -> s {additionalMetricsToRetain = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated version of the security profile.
updateSecurityProfileResponse_version :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Integer)
updateSecurityProfileResponse_version = Lens.lens (\UpdateSecurityProfileResponse' {version} -> version) (\s@UpdateSecurityProfileResponse' {} a -> s {version = a} :: UpdateSecurityProfileResponse)

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
updateSecurityProfileResponse_behaviors :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe [Behavior])
updateSecurityProfileResponse_behaviors = Lens.lens (\UpdateSecurityProfileResponse' {behaviors} -> behaviors) (\s@UpdateSecurityProfileResponse' {} a -> s {behaviors = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateSecurityProfileResponse_httpStatus :: Lens.Lens' UpdateSecurityProfileResponse Prelude.Int
updateSecurityProfileResponse_httpStatus = Lens.lens (\UpdateSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityProfileResponse' {} a -> s {httpStatus = a} :: UpdateSecurityProfileResponse)

instance Prelude.NFData UpdateSecurityProfileResponse where
  rnf UpdateSecurityProfileResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf alertTargets
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf securityProfileDescription
      `Prelude.seq` Prelude.rnf additionalMetricsToRetainV2
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf securityProfileArn
      `Prelude.seq` Prelude.rnf additionalMetricsToRetain
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf behaviors
      `Prelude.seq` Prelude.rnf httpStatus
