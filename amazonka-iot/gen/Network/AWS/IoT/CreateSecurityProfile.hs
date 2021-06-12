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
-- Module      : Network.AWS.IoT.CreateSecurityProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender security profile.
module Network.AWS.IoT.CreateSecurityProfile
  ( -- * Creating a Request
    CreateSecurityProfile (..),
    newCreateSecurityProfile,

    -- * Request Lenses
    createSecurityProfile_alertTargets,
    createSecurityProfile_additionalMetricsToRetain,
    createSecurityProfile_behaviors,
    createSecurityProfile_additionalMetricsToRetainV2,
    createSecurityProfile_tags,
    createSecurityProfile_securityProfileDescription,
    createSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    CreateSecurityProfileResponse (..),
    newCreateSecurityProfileResponse,

    -- * Response Lenses
    createSecurityProfileResponse_securityProfileName,
    createSecurityProfileResponse_securityProfileArn,
    createSecurityProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSecurityProfile' smart constructor.
data CreateSecurityProfile = CreateSecurityProfile'
  { -- | Specifies the destinations to which alerts are sent. (Alerts are always
    -- sent to the console.) Alerts are generated when a device (thing)
    -- violates a behavior.
    alertTargets :: Core.Maybe (Core.HashMap AlertTargetType AlertTarget),
    -- | /Please use CreateSecurityProfileRequest$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here. Can be used with custom
    -- metrics; cannot be used with dimensions.
    additionalMetricsToRetain :: Core.Maybe [Core.Text],
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Core.Maybe [Behavior],
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here. Can be used with custom
    -- metrics; cannot be used with dimensions.
    additionalMetricsToRetainV2 :: Core.Maybe [MetricToRetain],
    -- | Metadata that can be used to manage the security profile.
    tags :: Core.Maybe [Tag],
    -- | A description of the security profile.
    securityProfileDescription :: Core.Maybe Core.Text,
    -- | The name you are giving to the security profile.
    securityProfileName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alertTargets', 'createSecurityProfile_alertTargets' - Specifies the destinations to which alerts are sent. (Alerts are always
-- sent to the console.) Alerts are generated when a device (thing)
-- violates a behavior.
--
-- 'additionalMetricsToRetain', 'createSecurityProfile_additionalMetricsToRetain' - /Please use CreateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
--
-- 'behaviors', 'createSecurityProfile_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'additionalMetricsToRetainV2', 'createSecurityProfile_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
--
-- 'tags', 'createSecurityProfile_tags' - Metadata that can be used to manage the security profile.
--
-- 'securityProfileDescription', 'createSecurityProfile_securityProfileDescription' - A description of the security profile.
--
-- 'securityProfileName', 'createSecurityProfile_securityProfileName' - The name you are giving to the security profile.
newCreateSecurityProfile ::
  -- | 'securityProfileName'
  Core.Text ->
  CreateSecurityProfile
newCreateSecurityProfile pSecurityProfileName_ =
  CreateSecurityProfile'
    { alertTargets = Core.Nothing,
      additionalMetricsToRetain = Core.Nothing,
      behaviors = Core.Nothing,
      additionalMetricsToRetainV2 = Core.Nothing,
      tags = Core.Nothing,
      securityProfileDescription = Core.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | Specifies the destinations to which alerts are sent. (Alerts are always
-- sent to the console.) Alerts are generated when a device (thing)
-- violates a behavior.
createSecurityProfile_alertTargets :: Lens.Lens' CreateSecurityProfile (Core.Maybe (Core.HashMap AlertTargetType AlertTarget))
createSecurityProfile_alertTargets = Lens.lens (\CreateSecurityProfile' {alertTargets} -> alertTargets) (\s@CreateSecurityProfile' {} a -> s {alertTargets = a} :: CreateSecurityProfile) Core.. Lens.mapping Lens._Coerce

-- | /Please use CreateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
createSecurityProfile_additionalMetricsToRetain :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Core.Text])
createSecurityProfile_additionalMetricsToRetain = Lens.lens (\CreateSecurityProfile' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@CreateSecurityProfile' {} a -> s {additionalMetricsToRetain = a} :: CreateSecurityProfile) Core.. Lens.mapping Lens._Coerce

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
createSecurityProfile_behaviors :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Behavior])
createSecurityProfile_behaviors = Lens.lens (\CreateSecurityProfile' {behaviors} -> behaviors) (\s@CreateSecurityProfile' {} a -> s {behaviors = a} :: CreateSecurityProfile) Core.. Lens.mapping Lens._Coerce

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
createSecurityProfile_additionalMetricsToRetainV2 :: Lens.Lens' CreateSecurityProfile (Core.Maybe [MetricToRetain])
createSecurityProfile_additionalMetricsToRetainV2 = Lens.lens (\CreateSecurityProfile' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@CreateSecurityProfile' {} a -> s {additionalMetricsToRetainV2 = a} :: CreateSecurityProfile) Core.. Lens.mapping Lens._Coerce

-- | Metadata that can be used to manage the security profile.
createSecurityProfile_tags :: Lens.Lens' CreateSecurityProfile (Core.Maybe [Tag])
createSecurityProfile_tags = Lens.lens (\CreateSecurityProfile' {tags} -> tags) (\s@CreateSecurityProfile' {} a -> s {tags = a} :: CreateSecurityProfile) Core.. Lens.mapping Lens._Coerce

-- | A description of the security profile.
createSecurityProfile_securityProfileDescription :: Lens.Lens' CreateSecurityProfile (Core.Maybe Core.Text)
createSecurityProfile_securityProfileDescription = Lens.lens (\CreateSecurityProfile' {securityProfileDescription} -> securityProfileDescription) (\s@CreateSecurityProfile' {} a -> s {securityProfileDescription = a} :: CreateSecurityProfile)

-- | The name you are giving to the security profile.
createSecurityProfile_securityProfileName :: Lens.Lens' CreateSecurityProfile Core.Text
createSecurityProfile_securityProfileName = Lens.lens (\CreateSecurityProfile' {securityProfileName} -> securityProfileName) (\s@CreateSecurityProfile' {} a -> s {securityProfileName = a} :: CreateSecurityProfile)

instance Core.AWSRequest CreateSecurityProfile where
  type
    AWSResponse CreateSecurityProfile =
      CreateSecurityProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityProfileResponse'
            Core.<$> (x Core..?> "securityProfileName")
            Core.<*> (x Core..?> "securityProfileArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSecurityProfile

instance Core.NFData CreateSecurityProfile

instance Core.ToHeaders CreateSecurityProfile where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateSecurityProfile where
  toJSON CreateSecurityProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("alertTargets" Core..=) Core.<$> alertTargets,
            ("additionalMetricsToRetain" Core..=)
              Core.<$> additionalMetricsToRetain,
            ("behaviors" Core..=) Core.<$> behaviors,
            ("additionalMetricsToRetainV2" Core..=)
              Core.<$> additionalMetricsToRetainV2,
            ("tags" Core..=) Core.<$> tags,
            ("securityProfileDescription" Core..=)
              Core.<$> securityProfileDescription
          ]
      )

instance Core.ToPath CreateSecurityProfile where
  toPath CreateSecurityProfile' {..} =
    Core.mconcat
      [ "/security-profiles/",
        Core.toBS securityProfileName
      ]

instance Core.ToQuery CreateSecurityProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSecurityProfileResponse' smart constructor.
data CreateSecurityProfileResponse = CreateSecurityProfileResponse'
  { -- | The name you gave to the security profile.
    securityProfileName :: Core.Maybe Core.Text,
    -- | The ARN of the security profile.
    securityProfileArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileName', 'createSecurityProfileResponse_securityProfileName' - The name you gave to the security profile.
--
-- 'securityProfileArn', 'createSecurityProfileResponse_securityProfileArn' - The ARN of the security profile.
--
-- 'httpStatus', 'createSecurityProfileResponse_httpStatus' - The response's http status code.
newCreateSecurityProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSecurityProfileResponse
newCreateSecurityProfileResponse pHttpStatus_ =
  CreateSecurityProfileResponse'
    { securityProfileName =
        Core.Nothing,
      securityProfileArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name you gave to the security profile.
createSecurityProfileResponse_securityProfileName :: Lens.Lens' CreateSecurityProfileResponse (Core.Maybe Core.Text)
createSecurityProfileResponse_securityProfileName = Lens.lens (\CreateSecurityProfileResponse' {securityProfileName} -> securityProfileName) (\s@CreateSecurityProfileResponse' {} a -> s {securityProfileName = a} :: CreateSecurityProfileResponse)

-- | The ARN of the security profile.
createSecurityProfileResponse_securityProfileArn :: Lens.Lens' CreateSecurityProfileResponse (Core.Maybe Core.Text)
createSecurityProfileResponse_securityProfileArn = Lens.lens (\CreateSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@CreateSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: CreateSecurityProfileResponse)

-- | The response's http status code.
createSecurityProfileResponse_httpStatus :: Lens.Lens' CreateSecurityProfileResponse Core.Int
createSecurityProfileResponse_httpStatus = Lens.lens (\CreateSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityProfileResponse' {} a -> s {httpStatus = a} :: CreateSecurityProfileResponse)

instance Core.NFData CreateSecurityProfileResponse
