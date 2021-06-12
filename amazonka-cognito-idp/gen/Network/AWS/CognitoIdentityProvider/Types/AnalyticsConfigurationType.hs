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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Amazon Pinpoint analytics configuration for collecting metrics for a
-- user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
--
-- /See:/ 'newAnalyticsConfigurationType' smart constructor.
data AnalyticsConfigurationType = AnalyticsConfigurationType'
  { -- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
    -- use the Amazon Pinpoint project for Pinpoint integration with the chosen
    -- User Pool Client. Amazon Cognito publishes events to the pinpoint
    -- project declared by the app ARN.
    applicationArn :: Core.Maybe Core.Text,
    -- | The application ID for an Amazon Pinpoint application.
    applicationId :: Core.Maybe Core.Text,
    -- | The ARN of an IAM role that authorizes Amazon Cognito to publish events
    -- to Amazon Pinpoint analytics.
    roleArn :: Core.Maybe Core.Text,
    -- | If @UserDataShared@ is @true@, Amazon Cognito will include user data in
    -- the events it publishes to Amazon Pinpoint analytics.
    userDataShared :: Core.Maybe Core.Bool,
    -- | The external ID.
    externalId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalyticsConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'analyticsConfigurationType_applicationArn' - The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
-- use the Amazon Pinpoint project for Pinpoint integration with the chosen
-- User Pool Client. Amazon Cognito publishes events to the pinpoint
-- project declared by the app ARN.
--
-- 'applicationId', 'analyticsConfigurationType_applicationId' - The application ID for an Amazon Pinpoint application.
--
-- 'roleArn', 'analyticsConfigurationType_roleArn' - The ARN of an IAM role that authorizes Amazon Cognito to publish events
-- to Amazon Pinpoint analytics.
--
-- 'userDataShared', 'analyticsConfigurationType_userDataShared' - If @UserDataShared@ is @true@, Amazon Cognito will include user data in
-- the events it publishes to Amazon Pinpoint analytics.
--
-- 'externalId', 'analyticsConfigurationType_externalId' - The external ID.
newAnalyticsConfigurationType ::
  AnalyticsConfigurationType
newAnalyticsConfigurationType =
  AnalyticsConfigurationType'
    { applicationArn =
        Core.Nothing,
      applicationId = Core.Nothing,
      roleArn = Core.Nothing,
      userDataShared = Core.Nothing,
      externalId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
-- use the Amazon Pinpoint project for Pinpoint integration with the chosen
-- User Pool Client. Amazon Cognito publishes events to the pinpoint
-- project declared by the app ARN.
analyticsConfigurationType_applicationArn :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Core.Text)
analyticsConfigurationType_applicationArn = Lens.lens (\AnalyticsConfigurationType' {applicationArn} -> applicationArn) (\s@AnalyticsConfigurationType' {} a -> s {applicationArn = a} :: AnalyticsConfigurationType)

-- | The application ID for an Amazon Pinpoint application.
analyticsConfigurationType_applicationId :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Core.Text)
analyticsConfigurationType_applicationId = Lens.lens (\AnalyticsConfigurationType' {applicationId} -> applicationId) (\s@AnalyticsConfigurationType' {} a -> s {applicationId = a} :: AnalyticsConfigurationType)

-- | The ARN of an IAM role that authorizes Amazon Cognito to publish events
-- to Amazon Pinpoint analytics.
analyticsConfigurationType_roleArn :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Core.Text)
analyticsConfigurationType_roleArn = Lens.lens (\AnalyticsConfigurationType' {roleArn} -> roleArn) (\s@AnalyticsConfigurationType' {} a -> s {roleArn = a} :: AnalyticsConfigurationType)

-- | If @UserDataShared@ is @true@, Amazon Cognito will include user data in
-- the events it publishes to Amazon Pinpoint analytics.
analyticsConfigurationType_userDataShared :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Core.Bool)
analyticsConfigurationType_userDataShared = Lens.lens (\AnalyticsConfigurationType' {userDataShared} -> userDataShared) (\s@AnalyticsConfigurationType' {} a -> s {userDataShared = a} :: AnalyticsConfigurationType)

-- | The external ID.
analyticsConfigurationType_externalId :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Core.Text)
analyticsConfigurationType_externalId = Lens.lens (\AnalyticsConfigurationType' {externalId} -> externalId) (\s@AnalyticsConfigurationType' {} a -> s {externalId = a} :: AnalyticsConfigurationType)

instance Core.FromJSON AnalyticsConfigurationType where
  parseJSON =
    Core.withObject
      "AnalyticsConfigurationType"
      ( \x ->
          AnalyticsConfigurationType'
            Core.<$> (x Core..:? "ApplicationArn")
            Core.<*> (x Core..:? "ApplicationId")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "UserDataShared")
            Core.<*> (x Core..:? "ExternalId")
      )

instance Core.Hashable AnalyticsConfigurationType

instance Core.NFData AnalyticsConfigurationType

instance Core.ToJSON AnalyticsConfigurationType where
  toJSON AnalyticsConfigurationType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApplicationArn" Core..=) Core.<$> applicationArn,
            ("ApplicationId" Core..=) Core.<$> applicationId,
            ("RoleArn" Core..=) Core.<$> roleArn,
            ("UserDataShared" Core..=) Core.<$> userDataShared,
            ("ExternalId" Core..=) Core.<$> externalId
          ]
      )
