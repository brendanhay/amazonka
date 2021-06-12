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
-- Module      : Network.AWS.IoT.DescribeSecurityProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender security profile.
module Network.AWS.IoT.DescribeSecurityProfile
  ( -- * Creating a Request
    DescribeSecurityProfile (..),
    newDescribeSecurityProfile,

    -- * Request Lenses
    describeSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    DescribeSecurityProfileResponse (..),
    newDescribeSecurityProfileResponse,

    -- * Response Lenses
    describeSecurityProfileResponse_lastModifiedDate,
    describeSecurityProfileResponse_alertTargets,
    describeSecurityProfileResponse_additionalMetricsToRetain,
    describeSecurityProfileResponse_creationDate,
    describeSecurityProfileResponse_version,
    describeSecurityProfileResponse_securityProfileName,
    describeSecurityProfileResponse_behaviors,
    describeSecurityProfileResponse_additionalMetricsToRetainV2,
    describeSecurityProfileResponse_securityProfileDescription,
    describeSecurityProfileResponse_securityProfileArn,
    describeSecurityProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSecurityProfile' smart constructor.
data DescribeSecurityProfile = DescribeSecurityProfile'
  { -- | The name of the security profile whose information you want to get.
    securityProfileName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileName', 'describeSecurityProfile_securityProfileName' - The name of the security profile whose information you want to get.
newDescribeSecurityProfile ::
  -- | 'securityProfileName'
  Core.Text ->
  DescribeSecurityProfile
newDescribeSecurityProfile pSecurityProfileName_ =
  DescribeSecurityProfile'
    { securityProfileName =
        pSecurityProfileName_
    }

-- | The name of the security profile whose information you want to get.
describeSecurityProfile_securityProfileName :: Lens.Lens' DescribeSecurityProfile Core.Text
describeSecurityProfile_securityProfileName = Lens.lens (\DescribeSecurityProfile' {securityProfileName} -> securityProfileName) (\s@DescribeSecurityProfile' {} a -> s {securityProfileName = a} :: DescribeSecurityProfile)

instance Core.AWSRequest DescribeSecurityProfile where
  type
    AWSResponse DescribeSecurityProfile =
      DescribeSecurityProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityProfileResponse'
            Core.<$> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "alertTargets" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "additionalMetricsToRetain"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "version")
            Core.<*> (x Core..?> "securityProfileName")
            Core.<*> (x Core..?> "behaviors" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "additionalMetricsToRetainV2"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "securityProfileDescription")
            Core.<*> (x Core..?> "securityProfileArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSecurityProfile

instance Core.NFData DescribeSecurityProfile

instance Core.ToHeaders DescribeSecurityProfile where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSecurityProfile where
  toPath DescribeSecurityProfile' {..} =
    Core.mconcat
      [ "/security-profiles/",
        Core.toBS securityProfileName
      ]

instance Core.ToQuery DescribeSecurityProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'
  { -- | The time the security profile was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Core.Maybe (Core.HashMap AlertTargetType AlertTarget),
    -- | /Please use DescribeSecurityProfileResponse$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here.
    additionalMetricsToRetain :: Core.Maybe [Core.Text],
    -- | The time the security profile was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The version of the security profile. A new version is generated whenever
    -- the security profile is updated.
    version :: Core.Maybe Core.Integer,
    -- | The name of the security profile.
    securityProfileName :: Core.Maybe Core.Text,
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Core.Maybe [Behavior],
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s behaviors, but it is also
    -- retained for any metric specified here.
    additionalMetricsToRetainV2 :: Core.Maybe [MetricToRetain],
    -- | A description of the security profile (associated with the security
    -- profile when it was created or updated).
    securityProfileDescription :: Core.Maybe Core.Text,
    -- | The ARN of the security profile.
    securityProfileArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'describeSecurityProfileResponse_lastModifiedDate' - The time the security profile was last modified.
--
-- 'alertTargets', 'describeSecurityProfileResponse_alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- 'additionalMetricsToRetain', 'describeSecurityProfileResponse_additionalMetricsToRetain' - /Please use DescribeSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here.
--
-- 'creationDate', 'describeSecurityProfileResponse_creationDate' - The time the security profile was created.
--
-- 'version', 'describeSecurityProfileResponse_version' - The version of the security profile. A new version is generated whenever
-- the security profile is updated.
--
-- 'securityProfileName', 'describeSecurityProfileResponse_securityProfileName' - The name of the security profile.
--
-- 'behaviors', 'describeSecurityProfileResponse_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'additionalMetricsToRetainV2', 'describeSecurityProfileResponse_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here.
--
-- 'securityProfileDescription', 'describeSecurityProfileResponse_securityProfileDescription' - A description of the security profile (associated with the security
-- profile when it was created or updated).
--
-- 'securityProfileArn', 'describeSecurityProfileResponse_securityProfileArn' - The ARN of the security profile.
--
-- 'httpStatus', 'describeSecurityProfileResponse_httpStatus' - The response's http status code.
newDescribeSecurityProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSecurityProfileResponse
newDescribeSecurityProfileResponse pHttpStatus_ =
  DescribeSecurityProfileResponse'
    { lastModifiedDate =
        Core.Nothing,
      alertTargets = Core.Nothing,
      additionalMetricsToRetain = Core.Nothing,
      creationDate = Core.Nothing,
      version = Core.Nothing,
      securityProfileName = Core.Nothing,
      behaviors = Core.Nothing,
      additionalMetricsToRetainV2 = Core.Nothing,
      securityProfileDescription = Core.Nothing,
      securityProfileArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the security profile was last modified.
describeSecurityProfileResponse_lastModifiedDate :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.UTCTime)
describeSecurityProfileResponse_lastModifiedDate = Lens.lens (\DescribeSecurityProfileResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeSecurityProfileResponse' {} a -> s {lastModifiedDate = a} :: DescribeSecurityProfileResponse) Core.. Lens.mapping Core._Time

-- | Where the alerts are sent. (Alerts are always sent to the console.)
describeSecurityProfileResponse_alertTargets :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe (Core.HashMap AlertTargetType AlertTarget))
describeSecurityProfileResponse_alertTargets = Lens.lens (\DescribeSecurityProfileResponse' {alertTargets} -> alertTargets) (\s@DescribeSecurityProfileResponse' {} a -> s {alertTargets = a} :: DescribeSecurityProfileResponse) Core.. Lens.mapping Lens._Coerce

-- | /Please use DescribeSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here.
describeSecurityProfileResponse_additionalMetricsToRetain :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe [Core.Text])
describeSecurityProfileResponse_additionalMetricsToRetain = Lens.lens (\DescribeSecurityProfileResponse' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@DescribeSecurityProfileResponse' {} a -> s {additionalMetricsToRetain = a} :: DescribeSecurityProfileResponse) Core.. Lens.mapping Lens._Coerce

-- | The time the security profile was created.
describeSecurityProfileResponse_creationDate :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.UTCTime)
describeSecurityProfileResponse_creationDate = Lens.lens (\DescribeSecurityProfileResponse' {creationDate} -> creationDate) (\s@DescribeSecurityProfileResponse' {} a -> s {creationDate = a} :: DescribeSecurityProfileResponse) Core.. Lens.mapping Core._Time

-- | The version of the security profile. A new version is generated whenever
-- the security profile is updated.
describeSecurityProfileResponse_version :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.Integer)
describeSecurityProfileResponse_version = Lens.lens (\DescribeSecurityProfileResponse' {version} -> version) (\s@DescribeSecurityProfileResponse' {} a -> s {version = a} :: DescribeSecurityProfileResponse)

-- | The name of the security profile.
describeSecurityProfileResponse_securityProfileName :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.Text)
describeSecurityProfileResponse_securityProfileName = Lens.lens (\DescribeSecurityProfileResponse' {securityProfileName} -> securityProfileName) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfileName = a} :: DescribeSecurityProfileResponse)

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
describeSecurityProfileResponse_behaviors :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe [Behavior])
describeSecurityProfileResponse_behaviors = Lens.lens (\DescribeSecurityProfileResponse' {behaviors} -> behaviors) (\s@DescribeSecurityProfileResponse' {} a -> s {behaviors = a} :: DescribeSecurityProfileResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here.
describeSecurityProfileResponse_additionalMetricsToRetainV2 :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe [MetricToRetain])
describeSecurityProfileResponse_additionalMetricsToRetainV2 = Lens.lens (\DescribeSecurityProfileResponse' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@DescribeSecurityProfileResponse' {} a -> s {additionalMetricsToRetainV2 = a} :: DescribeSecurityProfileResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the security profile (associated with the security
-- profile when it was created or updated).
describeSecurityProfileResponse_securityProfileDescription :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.Text)
describeSecurityProfileResponse_securityProfileDescription = Lens.lens (\DescribeSecurityProfileResponse' {securityProfileDescription} -> securityProfileDescription) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfileDescription = a} :: DescribeSecurityProfileResponse)

-- | The ARN of the security profile.
describeSecurityProfileResponse_securityProfileArn :: Lens.Lens' DescribeSecurityProfileResponse (Core.Maybe Core.Text)
describeSecurityProfileResponse_securityProfileArn = Lens.lens (\DescribeSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: DescribeSecurityProfileResponse)

-- | The response's http status code.
describeSecurityProfileResponse_httpStatus :: Lens.Lens' DescribeSecurityProfileResponse Core.Int
describeSecurityProfileResponse_httpStatus = Lens.lens (\DescribeSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityProfileResponse' {} a -> s {httpStatus = a} :: DescribeSecurityProfileResponse)

instance Core.NFData DescribeSecurityProfileResponse
