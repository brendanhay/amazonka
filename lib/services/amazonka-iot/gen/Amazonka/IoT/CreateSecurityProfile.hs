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
-- Module      : Amazonka.IoT.CreateSecurityProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender security profile.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateSecurityProfile>
-- action.
module Amazonka.IoT.CreateSecurityProfile
  ( -- * Creating a Request
    CreateSecurityProfile (..),
    newCreateSecurityProfile,

    -- * Request Lenses
    createSecurityProfile_tags,
    createSecurityProfile_alertTargets,
    createSecurityProfile_securityProfileDescription,
    createSecurityProfile_additionalMetricsToRetainV2,
    createSecurityProfile_additionalMetricsToRetain,
    createSecurityProfile_behaviors,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSecurityProfile' smart constructor.
data CreateSecurityProfile = CreateSecurityProfile'
  { -- | Metadata that can be used to manage the security profile.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the destinations to which alerts are sent. (Alerts are always
    -- sent to the console.) Alerts are generated when a device (thing)
    -- violates a behavior.
    alertTargets :: Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget),
    -- | A description of the security profile.
    securityProfileDescription :: Prelude.Maybe Prelude.Text,
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here. Can be used with custom
    -- metrics; cannot be used with dimensions.
    additionalMetricsToRetainV2 :: Prelude.Maybe [MetricToRetain],
    -- | /Please use CreateSecurityProfileRequest$additionalMetricsToRetainV2
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
    -- | The name you are giving to the security profile.
    securityProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSecurityProfile_tags' - Metadata that can be used to manage the security profile.
--
-- 'alertTargets', 'createSecurityProfile_alertTargets' - Specifies the destinations to which alerts are sent. (Alerts are always
-- sent to the console.) Alerts are generated when a device (thing)
-- violates a behavior.
--
-- 'securityProfileDescription', 'createSecurityProfile_securityProfileDescription' - A description of the security profile.
--
-- 'additionalMetricsToRetainV2', 'createSecurityProfile_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
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
-- 'securityProfileName', 'createSecurityProfile_securityProfileName' - The name you are giving to the security profile.
newCreateSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  CreateSecurityProfile
newCreateSecurityProfile pSecurityProfileName_ =
  CreateSecurityProfile'
    { tags = Prelude.Nothing,
      alertTargets = Prelude.Nothing,
      securityProfileDescription = Prelude.Nothing,
      additionalMetricsToRetainV2 = Prelude.Nothing,
      additionalMetricsToRetain = Prelude.Nothing,
      behaviors = Prelude.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | Metadata that can be used to manage the security profile.
createSecurityProfile_tags :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe [Tag])
createSecurityProfile_tags = Lens.lens (\CreateSecurityProfile' {tags} -> tags) (\s@CreateSecurityProfile' {} a -> s {tags = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the destinations to which alerts are sent. (Alerts are always
-- sent to the console.) Alerts are generated when a device (thing)
-- violates a behavior.
createSecurityProfile_alertTargets :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget))
createSecurityProfile_alertTargets = Lens.lens (\CreateSecurityProfile' {alertTargets} -> alertTargets) (\s@CreateSecurityProfile' {} a -> s {alertTargets = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | A description of the security profile.
createSecurityProfile_securityProfileDescription :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe Prelude.Text)
createSecurityProfile_securityProfileDescription = Lens.lens (\CreateSecurityProfile' {securityProfileDescription} -> securityProfileDescription) (\s@CreateSecurityProfile' {} a -> s {securityProfileDescription = a} :: CreateSecurityProfile)

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
createSecurityProfile_additionalMetricsToRetainV2 :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe [MetricToRetain])
createSecurityProfile_additionalMetricsToRetainV2 = Lens.lens (\CreateSecurityProfile' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@CreateSecurityProfile' {} a -> s {additionalMetricsToRetainV2 = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | /Please use CreateSecurityProfileRequest$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here. Can be used with custom
-- metrics; cannot be used with dimensions.
createSecurityProfile_additionalMetricsToRetain :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe [Prelude.Text])
createSecurityProfile_additionalMetricsToRetain = Lens.lens (\CreateSecurityProfile' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@CreateSecurityProfile' {} a -> s {additionalMetricsToRetain = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
createSecurityProfile_behaviors :: Lens.Lens' CreateSecurityProfile (Prelude.Maybe [Behavior])
createSecurityProfile_behaviors = Lens.lens (\CreateSecurityProfile' {behaviors} -> behaviors) (\s@CreateSecurityProfile' {} a -> s {behaviors = a} :: CreateSecurityProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name you are giving to the security profile.
createSecurityProfile_securityProfileName :: Lens.Lens' CreateSecurityProfile Prelude.Text
createSecurityProfile_securityProfileName = Lens.lens (\CreateSecurityProfile' {securityProfileName} -> securityProfileName) (\s@CreateSecurityProfile' {} a -> s {securityProfileName = a} :: CreateSecurityProfile)

instance Core.AWSRequest CreateSecurityProfile where
  type
    AWSResponse CreateSecurityProfile =
      CreateSecurityProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityProfileResponse'
            Prelude.<$> (x Data..?> "securityProfileName")
            Prelude.<*> (x Data..?> "securityProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecurityProfile where
  hashWithSalt _salt CreateSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` alertTargets
      `Prelude.hashWithSalt` securityProfileDescription
      `Prelude.hashWithSalt` additionalMetricsToRetainV2
      `Prelude.hashWithSalt` additionalMetricsToRetain
      `Prelude.hashWithSalt` behaviors
      `Prelude.hashWithSalt` securityProfileName

instance Prelude.NFData CreateSecurityProfile where
  rnf CreateSecurityProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf alertTargets
      `Prelude.seq` Prelude.rnf securityProfileDescription
      `Prelude.seq` Prelude.rnf additionalMetricsToRetainV2
      `Prelude.seq` Prelude.rnf additionalMetricsToRetain
      `Prelude.seq` Prelude.rnf behaviors
      `Prelude.seq` Prelude.rnf securityProfileName

instance Data.ToHeaders CreateSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateSecurityProfile where
  toJSON CreateSecurityProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("alertTargets" Data..=) Prelude.<$> alertTargets,
            ("securityProfileDescription" Data..=)
              Prelude.<$> securityProfileDescription,
            ("additionalMetricsToRetainV2" Data..=)
              Prelude.<$> additionalMetricsToRetainV2,
            ("additionalMetricsToRetain" Data..=)
              Prelude.<$> additionalMetricsToRetain,
            ("behaviors" Data..=) Prelude.<$> behaviors
          ]
      )

instance Data.ToPath CreateSecurityProfile where
  toPath CreateSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS securityProfileName
      ]

instance Data.ToQuery CreateSecurityProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecurityProfileResponse' smart constructor.
data CreateSecurityProfileResponse = CreateSecurityProfileResponse'
  { -- | The name you gave to the security profile.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the security profile.
    securityProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateSecurityProfileResponse
newCreateSecurityProfileResponse pHttpStatus_ =
  CreateSecurityProfileResponse'
    { securityProfileName =
        Prelude.Nothing,
      securityProfileArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name you gave to the security profile.
createSecurityProfileResponse_securityProfileName :: Lens.Lens' CreateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
createSecurityProfileResponse_securityProfileName = Lens.lens (\CreateSecurityProfileResponse' {securityProfileName} -> securityProfileName) (\s@CreateSecurityProfileResponse' {} a -> s {securityProfileName = a} :: CreateSecurityProfileResponse)

-- | The ARN of the security profile.
createSecurityProfileResponse_securityProfileArn :: Lens.Lens' CreateSecurityProfileResponse (Prelude.Maybe Prelude.Text)
createSecurityProfileResponse_securityProfileArn = Lens.lens (\CreateSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@CreateSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: CreateSecurityProfileResponse)

-- | The response's http status code.
createSecurityProfileResponse_httpStatus :: Lens.Lens' CreateSecurityProfileResponse Prelude.Int
createSecurityProfileResponse_httpStatus = Lens.lens (\CreateSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityProfileResponse' {} a -> s {httpStatus = a} :: CreateSecurityProfileResponse)

instance Prelude.NFData CreateSecurityProfileResponse where
  rnf CreateSecurityProfileResponse' {..} =
    Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf securityProfileArn
      `Prelude.seq` Prelude.rnf httpStatus
