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
-- Module      : Network.AWS.IoT.UpdateSecurityProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IoT.UpdateSecurityProfile
  ( -- * Creating a Request
    UpdateSecurityProfile (..),
    newUpdateSecurityProfile,

    -- * Request Lenses
    updateSecurityProfile_expectedVersion,
    updateSecurityProfile_alertTargets,
    updateSecurityProfile_deleteAdditionalMetricsToRetain,
    updateSecurityProfile_additionalMetricsToRetain,
    updateSecurityProfile_deleteAlertTargets,
    updateSecurityProfile_behaviors,
    updateSecurityProfile_additionalMetricsToRetainV2,
    updateSecurityProfile_deleteBehaviors,
    updateSecurityProfile_securityProfileDescription,
    updateSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    UpdateSecurityProfileResponse (..),
    newUpdateSecurityProfileResponse,

    -- * Response Lenses
    updateSecurityProfileResponse_lastModifiedDate,
    updateSecurityProfileResponse_alertTargets,
    updateSecurityProfileResponse_additionalMetricsToRetain,
    updateSecurityProfileResponse_creationDate,
    updateSecurityProfileResponse_securityProfileName,
    updateSecurityProfileResponse_version,
    updateSecurityProfileResponse_behaviors,
    updateSecurityProfileResponse_additionalMetricsToRetainV2,
    updateSecurityProfileResponse_securityProfileDescription,
    updateSecurityProfileResponse_securityProfileArn,
    updateSecurityProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSecurityProfile' smart constructor.
data UpdateSecurityProfile = UpdateSecurityProfile'
  { -- | The expected version of the security profile. A new version is generated
    -- whenever the security profile is updated. If you specify a value that is
    -- different from the actual version, a @VersionConflictException@ is
    -- thrown.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget),
    -- | If true, delete all @additionalMetricsToRetain@ defined for this
    -- security profile. If any @additionalMetricsToRetain@ are defined in the
    -- current invocation, an exception occurs.
    deleteAdditionalMetricsToRetain :: Prelude.Maybe Prelude.Bool,
    -- | /Please use UpdateSecurityProfileRequest$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here. Can be used with custom
    -- metrics; cannot be used with dimensions.
    additionalMetricsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | If true, delete all @alertTargets@ defined for this security profile. If
    -- any @alertTargets@ are defined in the current invocation, an exception
    -- occurs.
    deleteAlertTargets :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Prelude.Maybe [Behavior],
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s behaviors, but it is also
    -- retained for any metric specified here. Can be used with custom metrics;
    -- cannot be used with dimensions.
    additionalMetricsToRetainV2 :: Prelude.Maybe [MetricToRetain],
    -- | If true, delete all @behaviors@ defined for this security profile. If
    -- any @behaviors@ are defined in the current invocation, an exception
    -- occurs.
    deleteBehaviors :: Prelude.Maybe Prelude.Bool,
    -- | A description of the security profile.
    securityProfileDescription :: Prelude.Maybe Prelude.Text,
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
-- 'expectedVersion', 'updateSecurityProfile_expectedVersion' - The expected version of the security profile. A new version is generated
-- whenever the security profile is updated. If you specify a value that is
-- different from the actual version, a @VersionConflictException@ is
-- thrown.
--
-- 'alertTargets', 'updateSecurityProfile_alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- 'deleteAdditionalMetricsToRetain', 'updateSecurityProfile_deleteAdditionalMetricsToRetain' - If true, delete all @additionalMetricsToRetain@ defined for this
-- security profile. If any @additionalMetricsToRetain@ are defined in the
-- current invocation, an exception occurs.
--
-- 'additionalMetricsToRetain', 'updateSecurityProfile_additionalMetricsToRetain' - /Please use UpdateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
--
-- 'deleteAlertTargets', 'updateSecurityProfile_deleteAlertTargets' - If true, delete all @alertTargets@ defined for this security profile. If
-- any @alertTargets@ are defined in the current invocation, an exception
-- occurs.
--
-- 'behaviors', 'updateSecurityProfile_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'additionalMetricsToRetainV2', 'updateSecurityProfile_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
--
-- 'deleteBehaviors', 'updateSecurityProfile_deleteBehaviors' - If true, delete all @behaviors@ defined for this security profile. If
-- any @behaviors@ are defined in the current invocation, an exception
-- occurs.
--
-- 'securityProfileDescription', 'updateSecurityProfile_securityProfileDescription' - A description of the security profile.
--
-- 'securityProfileName', 'updateSecurityProfile_securityProfileName' - The name of the security profile you want to update.
newUpdateSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  UpdateSecurityProfile
newUpdateSecurityProfile pSecurityProfileName_ =
  UpdateSecurityProfile'
    { expectedVersion =
        Prelude.Nothing,
      alertTargets = Prelude.Nothing,
      deleteAdditionalMetricsToRetain = Prelude.Nothing,
      additionalMetricsToRetain = Prelude.Nothing,
      deleteAlertTargets = Prelude.Nothing,
      behaviors = Prelude.Nothing,
      additionalMetricsToRetainV2 = Prelude.Nothing,
      deleteBehaviors = Prelude.Nothing,
      securityProfileDescription = Prelude.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | The expected version of the security profile. A new version is generated
-- whenever the security profile is updated. If you specify a value that is
-- different from the actual version, a @VersionConflictException@ is
-- thrown.
updateSecurityProfile_expectedVersion :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Integer)
updateSecurityProfile_expectedVersion = Lens.lens (\UpdateSecurityProfile' {expectedVersion} -> expectedVersion) (\s@UpdateSecurityProfile' {} a -> s {expectedVersion = a} :: UpdateSecurityProfile)

-- | Where the alerts are sent. (Alerts are always sent to the console.)
updateSecurityProfile_alertTargets :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget))
updateSecurityProfile_alertTargets = Lens.lens (\UpdateSecurityProfile' {alertTargets} -> alertTargets) (\s@UpdateSecurityProfile' {} a -> s {alertTargets = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens._Coerce

-- | If true, delete all @additionalMetricsToRetain@ defined for this
-- security profile. If any @additionalMetricsToRetain@ are defined in the
-- current invocation, an exception occurs.
updateSecurityProfile_deleteAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Bool)
updateSecurityProfile_deleteAdditionalMetricsToRetain = Lens.lens (\UpdateSecurityProfile' {deleteAdditionalMetricsToRetain} -> deleteAdditionalMetricsToRetain) (\s@UpdateSecurityProfile' {} a -> s {deleteAdditionalMetricsToRetain = a} :: UpdateSecurityProfile)

-- | /Please use UpdateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
updateSecurityProfile_additionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [Prelude.Text])
updateSecurityProfile_additionalMetricsToRetain = Lens.lens (\UpdateSecurityProfile' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@UpdateSecurityProfile' {} a -> s {additionalMetricsToRetain = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens._Coerce

-- | If true, delete all @alertTargets@ defined for this security profile. If
-- any @alertTargets@ are defined in the current invocation, an exception
-- occurs.
updateSecurityProfile_deleteAlertTargets :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Bool)
updateSecurityProfile_deleteAlertTargets = Lens.lens (\UpdateSecurityProfile' {deleteAlertTargets} -> deleteAlertTargets) (\s@UpdateSecurityProfile' {} a -> s {deleteAlertTargets = a} :: UpdateSecurityProfile)

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
updateSecurityProfile_behaviors :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [Behavior])
updateSecurityProfile_behaviors = Lens.lens (\UpdateSecurityProfile' {behaviors} -> behaviors) (\s@UpdateSecurityProfile' {} a -> s {behaviors = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens._Coerce

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
updateSecurityProfile_additionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe [MetricToRetain])
updateSecurityProfile_additionalMetricsToRetainV2 = Lens.lens (\UpdateSecurityProfile' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@UpdateSecurityProfile' {} a -> s {additionalMetricsToRetainV2 = a} :: UpdateSecurityProfile) Prelude.. Lens.mapping Lens._Coerce

-- | If true, delete all @behaviors@ defined for this security profile. If
-- any @behaviors@ are defined in the current invocation, an exception
-- occurs.
updateSecurityProfile_deleteBehaviors :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Bool)
updateSecurityProfile_deleteBehaviors = Lens.lens (\UpdateSecurityProfile' {deleteBehaviors} -> deleteBehaviors) (\s@UpdateSecurityProfile' {} a -> s {deleteBehaviors = a} :: UpdateSecurityProfile)

-- | A description of the security profile.
updateSecurityProfile_securityProfileDescription :: Lens.Lens' UpdateSecurityProfile (Prelude.Maybe Prelude.Text)
updateSecurityProfile_securityProfileDescription = Lens.lens (\UpdateSecurityProfile' {securityProfileDescription} -> securityProfileDescription) (\s@UpdateSecurityProfile' {} a -> s {securityProfileDescription = a} :: UpdateSecurityProfile)

-- | The name of the security profile you want to update.
updateSecurityProfile_securityProfileName :: Lens.Lens' UpdateSecurityProfile Prelude.Text
updateSecurityProfile_securityProfileName = Lens.lens (\UpdateSecurityProfile' {securityProfileName} -> securityProfileName) (\s@UpdateSecurityProfile' {} a -> s {securityProfileName = a} :: UpdateSecurityProfile)

instance Core.AWSRequest UpdateSecurityProfile where
  type
    AWSResponse UpdateSecurityProfile =
      UpdateSecurityProfileResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecurityProfileResponse'
            Prelude.<$> (x Core..?> "lastModifiedDate")
            Prelude.<*> (x Core..?> "alertTargets" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "additionalMetricsToRetain"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "creationDate")
            Prelude.<*> (x Core..?> "securityProfileName")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "behaviors" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "additionalMetricsToRetainV2"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "securityProfileDescription")
            Prelude.<*> (x Core..?> "securityProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecurityProfile

instance Prelude.NFData UpdateSecurityProfile

instance Core.ToHeaders UpdateSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateSecurityProfile where
  toJSON UpdateSecurityProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("alertTargets" Core..=) Prelude.<$> alertTargets,
            ("deleteAdditionalMetricsToRetain" Core..=)
              Prelude.<$> deleteAdditionalMetricsToRetain,
            ("additionalMetricsToRetain" Core..=)
              Prelude.<$> additionalMetricsToRetain,
            ("deleteAlertTargets" Core..=)
              Prelude.<$> deleteAlertTargets,
            ("behaviors" Core..=) Prelude.<$> behaviors,
            ("additionalMetricsToRetainV2" Core..=)
              Prelude.<$> additionalMetricsToRetainV2,
            ("deleteBehaviors" Core..=)
              Prelude.<$> deleteBehaviors,
            ("securityProfileDescription" Core..=)
              Prelude.<$> securityProfileDescription
          ]
      )

instance Core.ToPath UpdateSecurityProfile where
  toPath UpdateSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Core.toBS securityProfileName
      ]

instance Core.ToQuery UpdateSecurityProfile where
  toQuery UpdateSecurityProfile' {..} =
    Prelude.mconcat
      ["expectedVersion" Core.=: expectedVersion]

-- | /See:/ 'newUpdateSecurityProfileResponse' smart constructor.
data UpdateSecurityProfileResponse = UpdateSecurityProfileResponse'
  { -- | The time the security profile was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget),
    -- | /Please use UpdateSecurityProfileResponse$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the security profile\'s @behaviors@, but
    -- it is also retained for any metric specified here.
    additionalMetricsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | The time the security profile was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the security profile that was updated.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The updated version of the security profile.
    version :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Prelude.Maybe [Behavior],
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s behaviors, but it is also
    -- retained for any metric specified here. Can be used with custom metrics;
    -- cannot be used with dimensions.
    additionalMetricsToRetainV2 :: Prelude.Maybe [MetricToRetain],
    -- | The description of the security profile.
    securityProfileDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the security profile that was updated.
    securityProfileArn :: Prelude.Maybe Prelude.Text,
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
-- 'additionalMetricsToRetain', 'updateSecurityProfileResponse_additionalMetricsToRetain' - /Please use UpdateSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the security profile\'s @behaviors@, but
-- it is also retained for any metric specified here.
--
-- 'creationDate', 'updateSecurityProfileResponse_creationDate' - The time the security profile was created.
--
-- 'securityProfileName', 'updateSecurityProfileResponse_securityProfileName' - The name of the security profile that was updated.
--
-- 'version', 'updateSecurityProfileResponse_version' - The updated version of the security profile.
--
-- 'behaviors', 'updateSecurityProfileResponse_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'additionalMetricsToRetainV2', 'updateSecurityProfileResponse_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
--
-- 'securityProfileDescription', 'updateSecurityProfileResponse_securityProfileDescription' - The description of the security profile.
--
-- 'securityProfileArn', 'updateSecurityProfileResponse_securityProfileArn' - The ARN of the security profile that was updated.
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
      additionalMetricsToRetain = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      version = Prelude.Nothing,
      behaviors = Prelude.Nothing,
      additionalMetricsToRetainV2 =
        Prelude.Nothing,
      securityProfileDescription = Prelude.Nothing,
      securityProfileArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the security profile was last modified.
updateSecurityProfileResponse_lastModifiedDate :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateSecurityProfileResponse_lastModifiedDate = Lens.lens (\UpdateSecurityProfileResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateSecurityProfileResponse' {} a -> s {lastModifiedDate = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Core._Time

-- | Where the alerts are sent. (Alerts are always sent to the console.)
updateSecurityProfileResponse_alertTargets :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget))
updateSecurityProfileResponse_alertTargets = Lens.lens (\UpdateSecurityProfileResponse' {alertTargets} -> alertTargets) (\s@UpdateSecurityProfileResponse' {} a -> s {alertTargets = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens._Coerce

-- | /Please use UpdateSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the security profile\'s @behaviors@, but
-- it is also retained for any metric specified here.
updateSecurityProfileResponse_additionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe [Prelude.Text])
updateSecurityProfileResponse_additionalMetricsToRetain = Lens.lens (\UpdateSecurityProfileResponse' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@UpdateSecurityProfileResponse' {} a -> s {additionalMetricsToRetain = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The time the security profile was created.
updateSecurityProfileResponse_creationDate :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.UTCTime)
updateSecurityProfileResponse_creationDate = Lens.lens (\UpdateSecurityProfileResponse' {creationDate} -> creationDate) (\s@UpdateSecurityProfileResponse' {} a -> s {creationDate = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the security profile that was updated.
updateSecurityProfileResponse_securityProfileName :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
updateSecurityProfileResponse_securityProfileName = Lens.lens (\UpdateSecurityProfileResponse' {securityProfileName} -> securityProfileName) (\s@UpdateSecurityProfileResponse' {} a -> s {securityProfileName = a} :: UpdateSecurityProfileResponse)

-- | The updated version of the security profile.
updateSecurityProfileResponse_version :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Integer)
updateSecurityProfileResponse_version = Lens.lens (\UpdateSecurityProfileResponse' {version} -> version) (\s@UpdateSecurityProfileResponse' {} a -> s {version = a} :: UpdateSecurityProfileResponse)

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
updateSecurityProfileResponse_behaviors :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe [Behavior])
updateSecurityProfileResponse_behaviors = Lens.lens (\UpdateSecurityProfileResponse' {behaviors} -> behaviors) (\s@UpdateSecurityProfileResponse' {} a -> s {behaviors = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here. Can be used with custom metrics;
-- cannot be used with dimensions.
updateSecurityProfileResponse_additionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe [MetricToRetain])
updateSecurityProfileResponse_additionalMetricsToRetainV2 = Lens.lens (\UpdateSecurityProfileResponse' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@UpdateSecurityProfileResponse' {} a -> s {additionalMetricsToRetainV2 = a} :: UpdateSecurityProfileResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The description of the security profile.
updateSecurityProfileResponse_securityProfileDescription :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
updateSecurityProfileResponse_securityProfileDescription = Lens.lens (\UpdateSecurityProfileResponse' {securityProfileDescription} -> securityProfileDescription) (\s@UpdateSecurityProfileResponse' {} a -> s {securityProfileDescription = a} :: UpdateSecurityProfileResponse)

-- | The ARN of the security profile that was updated.
updateSecurityProfileResponse_securityProfileArn :: Lens.Lens' UpdateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
updateSecurityProfileResponse_securityProfileArn = Lens.lens (\UpdateSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@UpdateSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: UpdateSecurityProfileResponse)

-- | The response's http status code.
updateSecurityProfileResponse_httpStatus :: Lens.Lens' UpdateSecurityProfileResponse Prelude.Int
updateSecurityProfileResponse_httpStatus = Lens.lens (\UpdateSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityProfileResponse' {} a -> s {httpStatus = a} :: UpdateSecurityProfileResponse)

instance Prelude.NFData UpdateSecurityProfileResponse
