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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AnalyticsConfigurationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AnalyticsConfigurationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Pinpoint analytics configuration necessary to collect metrics
-- for a user pool.
--
-- In Regions where Amazon Pinpointisn\'t available, user pools only
-- support sending events to Amazon Pinpoint projects in us-east-1. In
-- Regions where Amazon Pinpoint is available, user pools support sending
-- events to Amazon Pinpoint projects within that same Region.
--
-- /See:/ 'newAnalyticsConfigurationType' smart constructor.
data AnalyticsConfigurationType = AnalyticsConfigurationType'
  { -- | If @UserDataShared@ is @true@, Amazon Cognito includes user data in the
    -- events that it publishes to Amazon Pinpoint analytics.
    userDataShared :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of an Identity and Access Management role that authorizes Amazon
    -- Cognito to publish events to Amazon Pinpoint analytics.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
    -- use the Amazon Pinpoint project to integrate with the chosen user pool
    -- Client. Amazon Cognito publishes events to the Amazon Pinpoint project
    -- that the app ARN declares.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The external ID.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The application ID for an Amazon Pinpoint application.
    applicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyticsConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userDataShared', 'analyticsConfigurationType_userDataShared' - If @UserDataShared@ is @true@, Amazon Cognito includes user data in the
-- events that it publishes to Amazon Pinpoint analytics.
--
-- 'roleArn', 'analyticsConfigurationType_roleArn' - The ARN of an Identity and Access Management role that authorizes Amazon
-- Cognito to publish events to Amazon Pinpoint analytics.
--
-- 'applicationArn', 'analyticsConfigurationType_applicationArn' - The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
-- use the Amazon Pinpoint project to integrate with the chosen user pool
-- Client. Amazon Cognito publishes events to the Amazon Pinpoint project
-- that the app ARN declares.
--
-- 'externalId', 'analyticsConfigurationType_externalId' - The external ID.
--
-- 'applicationId', 'analyticsConfigurationType_applicationId' - The application ID for an Amazon Pinpoint application.
newAnalyticsConfigurationType ::
  AnalyticsConfigurationType
newAnalyticsConfigurationType =
  AnalyticsConfigurationType'
    { userDataShared =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      applicationArn = Prelude.Nothing,
      externalId = Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- | If @UserDataShared@ is @true@, Amazon Cognito includes user data in the
-- events that it publishes to Amazon Pinpoint analytics.
analyticsConfigurationType_userDataShared :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Bool)
analyticsConfigurationType_userDataShared = Lens.lens (\AnalyticsConfigurationType' {userDataShared} -> userDataShared) (\s@AnalyticsConfigurationType' {} a -> s {userDataShared = a} :: AnalyticsConfigurationType)

-- | The ARN of an Identity and Access Management role that authorizes Amazon
-- Cognito to publish events to Amazon Pinpoint analytics.
analyticsConfigurationType_roleArn :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_roleArn = Lens.lens (\AnalyticsConfigurationType' {roleArn} -> roleArn) (\s@AnalyticsConfigurationType' {} a -> s {roleArn = a} :: AnalyticsConfigurationType)

-- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can
-- use the Amazon Pinpoint project to integrate with the chosen user pool
-- Client. Amazon Cognito publishes events to the Amazon Pinpoint project
-- that the app ARN declares.
analyticsConfigurationType_applicationArn :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_applicationArn = Lens.lens (\AnalyticsConfigurationType' {applicationArn} -> applicationArn) (\s@AnalyticsConfigurationType' {} a -> s {applicationArn = a} :: AnalyticsConfigurationType)

-- | The external ID.
analyticsConfigurationType_externalId :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_externalId = Lens.lens (\AnalyticsConfigurationType' {externalId} -> externalId) (\s@AnalyticsConfigurationType' {} a -> s {externalId = a} :: AnalyticsConfigurationType)

-- | The application ID for an Amazon Pinpoint application.
analyticsConfigurationType_applicationId :: Lens.Lens' AnalyticsConfigurationType (Prelude.Maybe Prelude.Text)
analyticsConfigurationType_applicationId = Lens.lens (\AnalyticsConfigurationType' {applicationId} -> applicationId) (\s@AnalyticsConfigurationType' {} a -> s {applicationId = a} :: AnalyticsConfigurationType)

instance Core.FromJSON AnalyticsConfigurationType where
  parseJSON =
    Core.withObject
      "AnalyticsConfigurationType"
      ( \x ->
          AnalyticsConfigurationType'
            Prelude.<$> (x Core..:? "UserDataShared")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "ApplicationArn")
            Prelude.<*> (x Core..:? "ExternalId")
            Prelude.<*> (x Core..:? "ApplicationId")
      )

instance Prelude.Hashable AnalyticsConfigurationType where
  hashWithSalt _salt AnalyticsConfigurationType' {..} =
    _salt `Prelude.hashWithSalt` userDataShared
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` applicationArn
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData AnalyticsConfigurationType where
  rnf AnalyticsConfigurationType' {..} =
    Prelude.rnf userDataShared
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToJSON AnalyticsConfigurationType where
  toJSON AnalyticsConfigurationType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserDataShared" Core..=)
              Prelude.<$> userDataShared,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("ApplicationArn" Core..=)
              Prelude.<$> applicationArn,
            ("ExternalId" Core..=) Prelude.<$> externalId,
            ("ApplicationId" Core..=) Prelude.<$> applicationId
          ]
      )
