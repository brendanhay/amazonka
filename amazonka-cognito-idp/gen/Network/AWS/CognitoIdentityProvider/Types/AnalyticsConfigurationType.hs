{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The application ID for an Amazon Pinpoint application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an IAM role that authorizes Amazon Cognito to publish events
    -- to Amazon Pinpoint analytics.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | If @UserDataShared@ is @true@, Amazon Cognito will include user data in
    -- the events it publishes to Amazon Pinpoint analytics.
    userDataShared :: Prelude.Maybe Prelude.Bool,
    -- | The external ID.
    externalId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      applicationId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      userDataShared = Prelude.Nothing,
      externalId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
-- use the Amazon Pinpoint project for Pinpoint integration with the chosen
-- User Pool Client. Amazon Cognito publishes events to the pinpoint
-- project declared by the app ARN.
analyticsConfigurationType_applicationArn :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_applicationArn = Lens.lens (\AnalyticsConfigurationType' {applicationArn} -> applicationArn) (\s@AnalyticsConfigurationType' {} a -> s {applicationArn = a} :: AnalyticsConfigurationType)

-- | The application ID for an Amazon Pinpoint application.
analyticsConfigurationType_applicationId :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_applicationId = Lens.lens (\AnalyticsConfigurationType' {applicationId} -> applicationId) (\s@AnalyticsConfigurationType' {} a -> s {applicationId = a} :: AnalyticsConfigurationType)

-- | The ARN of an IAM role that authorizes Amazon Cognito to publish events
-- to Amazon Pinpoint analytics.
analyticsConfigurationType_roleArn :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_roleArn = Lens.lens (\AnalyticsConfigurationType' {roleArn} -> roleArn) (\s@AnalyticsConfigurationType' {} a -> s {roleArn = a} :: AnalyticsConfigurationType)

-- | If @UserDataShared@ is @true@, Amazon Cognito will include user data in
-- the events it publishes to Amazon Pinpoint analytics.
analyticsConfigurationType_userDataShared :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Bool)
analyticsConfigurationType_userDataShared = Lens.lens (\AnalyticsConfigurationType' {userDataShared} -> userDataShared) (\s@AnalyticsConfigurationType' {} a -> s {userDataShared = a} :: AnalyticsConfigurationType)

-- | The external ID.
analyticsConfigurationType_externalId :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_externalId = Lens.lens (\AnalyticsConfigurationType' {externalId} -> externalId) (\s@AnalyticsConfigurationType' {} a -> s {externalId = a} :: AnalyticsConfigurationType)

instance Prelude.FromJSON AnalyticsConfigurationType where
  parseJSON =
    Prelude.withObject
      "AnalyticsConfigurationType"
      ( \x ->
          AnalyticsConfigurationType'
            Prelude.<$> (x Prelude..:? "ApplicationArn")
            Prelude.<*> (x Prelude..:? "ApplicationId")
            Prelude.<*> (x Prelude..:? "RoleArn")
            Prelude.<*> (x Prelude..:? "UserDataShared")
            Prelude.<*> (x Prelude..:? "ExternalId")
      )

instance Prelude.Hashable AnalyticsConfigurationType

instance Prelude.NFData AnalyticsConfigurationType

instance Prelude.ToJSON AnalyticsConfigurationType where
  toJSON AnalyticsConfigurationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ApplicationArn" Prelude..=)
              Prelude.<$> applicationArn,
            ("ApplicationId" Prelude..=)
              Prelude.<$> applicationId,
            ("RoleArn" Prelude..=) Prelude.<$> roleArn,
            ("UserDataShared" Prelude..=)
              Prelude.<$> userDataShared,
            ("ExternalId" Prelude..=) Prelude.<$> externalId
          ]
      )
