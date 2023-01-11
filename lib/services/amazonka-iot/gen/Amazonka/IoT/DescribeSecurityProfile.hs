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
-- Module      : Amazonka.IoT.DescribeSecurityProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender security profile.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeSecurityProfile>
-- action.
module Amazonka.IoT.DescribeSecurityProfile
  ( -- * Creating a Request
    DescribeSecurityProfile (..),
    newDescribeSecurityProfile,

    -- * Request Lenses
    describeSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    DescribeSecurityProfileResponse (..),
    newDescribeSecurityProfileResponse,

    -- * Response Lenses
    describeSecurityProfileResponse_additionalMetricsToRetain,
    describeSecurityProfileResponse_additionalMetricsToRetainV2,
    describeSecurityProfileResponse_alertTargets,
    describeSecurityProfileResponse_behaviors,
    describeSecurityProfileResponse_creationDate,
    describeSecurityProfileResponse_lastModifiedDate,
    describeSecurityProfileResponse_securityProfileArn,
    describeSecurityProfileResponse_securityProfileDescription,
    describeSecurityProfileResponse_securityProfileName,
    describeSecurityProfileResponse_version,
    describeSecurityProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSecurityProfile' smart constructor.
data DescribeSecurityProfile = DescribeSecurityProfile'
  { -- | The name of the security profile whose information you want to get.
    securityProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeSecurityProfile
newDescribeSecurityProfile pSecurityProfileName_ =
  DescribeSecurityProfile'
    { securityProfileName =
        pSecurityProfileName_
    }

-- | The name of the security profile whose information you want to get.
describeSecurityProfile_securityProfileName :: Lens.Lens' DescribeSecurityProfile Prelude.Text
describeSecurityProfile_securityProfileName = Lens.lens (\DescribeSecurityProfile' {securityProfileName} -> securityProfileName) (\s@DescribeSecurityProfile' {} a -> s {securityProfileName = a} :: DescribeSecurityProfile)

instance Core.AWSRequest DescribeSecurityProfile where
  type
    AWSResponse DescribeSecurityProfile =
      DescribeSecurityProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityProfileResponse'
            Prelude.<$> ( x Data..?> "additionalMetricsToRetain"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "additionalMetricsToRetainV2"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "alertTargets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "behaviors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "securityProfileArn")
            Prelude.<*> (x Data..?> "securityProfileDescription")
            Prelude.<*> (x Data..?> "securityProfileName")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSecurityProfile where
  hashWithSalt _salt DescribeSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` securityProfileName

instance Prelude.NFData DescribeSecurityProfile where
  rnf DescribeSecurityProfile' {..} =
    Prelude.rnf securityProfileName

instance Data.ToHeaders DescribeSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSecurityProfile where
  toPath DescribeSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS securityProfileName
      ]

instance Data.ToQuery DescribeSecurityProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'
  { -- | /Please use DescribeSecurityProfileResponse$additionalMetricsToRetainV2
    -- instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s @behaviors@, but it is
    -- also retained for any metric specified here.
    additionalMetricsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | A list of metrics whose data is retained (stored). By default, data is
    -- retained for any metric used in the profile\'s behaviors, but it is also
    -- retained for any metric specified here.
    additionalMetricsToRetainV2 :: Prelude.Maybe [MetricToRetain],
    -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget),
    -- | Specifies the behaviors that, when violated by a device (thing), cause
    -- an alert.
    behaviors :: Prelude.Maybe [Behavior],
    -- | The time the security profile was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The time the security profile was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the security profile.
    securityProfileArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the security profile (associated with the security
    -- profile when it was created or updated).
    securityProfileDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the security profile.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The version of the security profile. A new version is generated whenever
    -- the security profile is updated.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalMetricsToRetain', 'describeSecurityProfileResponse_additionalMetricsToRetain' - /Please use DescribeSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here.
--
-- 'additionalMetricsToRetainV2', 'describeSecurityProfileResponse_additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here.
--
-- 'alertTargets', 'describeSecurityProfileResponse_alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- 'behaviors', 'describeSecurityProfileResponse_behaviors' - Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
--
-- 'creationDate', 'describeSecurityProfileResponse_creationDate' - The time the security profile was created.
--
-- 'lastModifiedDate', 'describeSecurityProfileResponse_lastModifiedDate' - The time the security profile was last modified.
--
-- 'securityProfileArn', 'describeSecurityProfileResponse_securityProfileArn' - The ARN of the security profile.
--
-- 'securityProfileDescription', 'describeSecurityProfileResponse_securityProfileDescription' - A description of the security profile (associated with the security
-- profile when it was created or updated).
--
-- 'securityProfileName', 'describeSecurityProfileResponse_securityProfileName' - The name of the security profile.
--
-- 'version', 'describeSecurityProfileResponse_version' - The version of the security profile. A new version is generated whenever
-- the security profile is updated.
--
-- 'httpStatus', 'describeSecurityProfileResponse_httpStatus' - The response's http status code.
newDescribeSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSecurityProfileResponse
newDescribeSecurityProfileResponse pHttpStatus_ =
  DescribeSecurityProfileResponse'
    { additionalMetricsToRetain =
        Prelude.Nothing,
      additionalMetricsToRetainV2 =
        Prelude.Nothing,
      alertTargets = Prelude.Nothing,
      behaviors = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      securityProfileArn = Prelude.Nothing,
      securityProfileDescription =
        Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | /Please use DescribeSecurityProfileResponse$additionalMetricsToRetainV2
-- instead./
--
-- A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s @behaviors@, but it is
-- also retained for any metric specified here.
describeSecurityProfileResponse_additionalMetricsToRetain :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe [Prelude.Text])
describeSecurityProfileResponse_additionalMetricsToRetain = Lens.lens (\DescribeSecurityProfileResponse' {additionalMetricsToRetain} -> additionalMetricsToRetain) (\s@DescribeSecurityProfileResponse' {} a -> s {additionalMetricsToRetain = a} :: DescribeSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of metrics whose data is retained (stored). By default, data is
-- retained for any metric used in the profile\'s behaviors, but it is also
-- retained for any metric specified here.
describeSecurityProfileResponse_additionalMetricsToRetainV2 :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe [MetricToRetain])
describeSecurityProfileResponse_additionalMetricsToRetainV2 = Lens.lens (\DescribeSecurityProfileResponse' {additionalMetricsToRetainV2} -> additionalMetricsToRetainV2) (\s@DescribeSecurityProfileResponse' {} a -> s {additionalMetricsToRetainV2 = a} :: DescribeSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | Where the alerts are sent. (Alerts are always sent to the console.)
describeSecurityProfileResponse_alertTargets :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe (Prelude.HashMap AlertTargetType AlertTarget))
describeSecurityProfileResponse_alertTargets = Lens.lens (\DescribeSecurityProfileResponse' {alertTargets} -> alertTargets) (\s@DescribeSecurityProfileResponse' {} a -> s {alertTargets = a} :: DescribeSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the behaviors that, when violated by a device (thing), cause
-- an alert.
describeSecurityProfileResponse_behaviors :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe [Behavior])
describeSecurityProfileResponse_behaviors = Lens.lens (\DescribeSecurityProfileResponse' {behaviors} -> behaviors) (\s@DescribeSecurityProfileResponse' {} a -> s {behaviors = a} :: DescribeSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time the security profile was created.
describeSecurityProfileResponse_creationDate :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeSecurityProfileResponse_creationDate = Lens.lens (\DescribeSecurityProfileResponse' {creationDate} -> creationDate) (\s@DescribeSecurityProfileResponse' {} a -> s {creationDate = a} :: DescribeSecurityProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The time the security profile was last modified.
describeSecurityProfileResponse_lastModifiedDate :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeSecurityProfileResponse_lastModifiedDate = Lens.lens (\DescribeSecurityProfileResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeSecurityProfileResponse' {} a -> s {lastModifiedDate = a} :: DescribeSecurityProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the security profile.
describeSecurityProfileResponse_securityProfileArn :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe Prelude.Text)
describeSecurityProfileResponse_securityProfileArn = Lens.lens (\DescribeSecurityProfileResponse' {securityProfileArn} -> securityProfileArn) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfileArn = a} :: DescribeSecurityProfileResponse)

-- | A description of the security profile (associated with the security
-- profile when it was created or updated).
describeSecurityProfileResponse_securityProfileDescription :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe Prelude.Text)
describeSecurityProfileResponse_securityProfileDescription = Lens.lens (\DescribeSecurityProfileResponse' {securityProfileDescription} -> securityProfileDescription) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfileDescription = a} :: DescribeSecurityProfileResponse)

-- | The name of the security profile.
describeSecurityProfileResponse_securityProfileName :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe Prelude.Text)
describeSecurityProfileResponse_securityProfileName = Lens.lens (\DescribeSecurityProfileResponse' {securityProfileName} -> securityProfileName) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfileName = a} :: DescribeSecurityProfileResponse)

-- | The version of the security profile. A new version is generated whenever
-- the security profile is updated.
describeSecurityProfileResponse_version :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe Prelude.Integer)
describeSecurityProfileResponse_version = Lens.lens (\DescribeSecurityProfileResponse' {version} -> version) (\s@DescribeSecurityProfileResponse' {} a -> s {version = a} :: DescribeSecurityProfileResponse)

-- | The response's http status code.
describeSecurityProfileResponse_httpStatus :: Lens.Lens' DescribeSecurityProfileResponse Prelude.Int
describeSecurityProfileResponse_httpStatus = Lens.lens (\DescribeSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityProfileResponse' {} a -> s {httpStatus = a} :: DescribeSecurityProfileResponse)

instance
  Prelude.NFData
    DescribeSecurityProfileResponse
  where
  rnf DescribeSecurityProfileResponse' {..} =
    Prelude.rnf additionalMetricsToRetain
      `Prelude.seq` Prelude.rnf additionalMetricsToRetainV2
      `Prelude.seq` Prelude.rnf alertTargets
      `Prelude.seq` Prelude.rnf behaviors
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf securityProfileArn
      `Prelude.seq` Prelude.rnf securityProfileDescription
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
