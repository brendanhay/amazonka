{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
  ( AnalyticsConfigurationType (..),

    -- * Smart constructor
    mkAnalyticsConfigurationType,

    -- * Lenses
    actApplicationARN,
    actUserDataShared,
    actApplicationId,
    actExternalId,
    actRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.
--
-- /See:/ 'mkAnalyticsConfigurationType' smart constructor.
data AnalyticsConfigurationType = AnalyticsConfigurationType'
  { -- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can use the Amazon Pinpoint project for Pinpoint integration with the chosen User Pool Client. Amazon Cognito publishes events to the pinpoint project declared by the app ARN.
    applicationARN :: Lude.Maybe Lude.Text,
    -- | If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
    userDataShared :: Lude.Maybe Lude.Bool,
    -- | The application ID for an Amazon Pinpoint application.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The external ID.
    externalId :: Lude.Maybe Lude.Text,
    -- | The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalyticsConfigurationType' with the minimum fields required to make a request.
--
-- * 'applicationARN' - The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can use the Amazon Pinpoint project for Pinpoint integration with the chosen User Pool Client. Amazon Cognito publishes events to the pinpoint project declared by the app ARN.
-- * 'userDataShared' - If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
-- * 'applicationId' - The application ID for an Amazon Pinpoint application.
-- * 'externalId' - The external ID.
-- * 'roleARN' - The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
mkAnalyticsConfigurationType ::
  AnalyticsConfigurationType
mkAnalyticsConfigurationType =
  AnalyticsConfigurationType'
    { applicationARN = Lude.Nothing,
      userDataShared = Lude.Nothing,
      applicationId = Lude.Nothing,
      externalId = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can use the Amazon Pinpoint project for Pinpoint integration with the chosen User Pool Client. Amazon Cognito publishes events to the pinpoint project declared by the app ARN.
--
-- /Note:/ Consider using 'applicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actApplicationARN :: Lens.Lens' AnalyticsConfigurationType (Lude.Maybe Lude.Text)
actApplicationARN = Lens.lens (applicationARN :: AnalyticsConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {applicationARN = a} :: AnalyticsConfigurationType)
{-# DEPRECATED actApplicationARN "Use generic-lens or generic-optics with 'applicationARN' instead." #-}

-- | If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
--
-- /Note:/ Consider using 'userDataShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actUserDataShared :: Lens.Lens' AnalyticsConfigurationType (Lude.Maybe Lude.Bool)
actUserDataShared = Lens.lens (userDataShared :: AnalyticsConfigurationType -> Lude.Maybe Lude.Bool) (\s a -> s {userDataShared = a} :: AnalyticsConfigurationType)
{-# DEPRECATED actUserDataShared "Use generic-lens or generic-optics with 'userDataShared' instead." #-}

-- | The application ID for an Amazon Pinpoint application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actApplicationId :: Lens.Lens' AnalyticsConfigurationType (Lude.Maybe Lude.Text)
actApplicationId = Lens.lens (applicationId :: AnalyticsConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: AnalyticsConfigurationType)
{-# DEPRECATED actApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The external ID.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actExternalId :: Lens.Lens' AnalyticsConfigurationType (Lude.Maybe Lude.Text)
actExternalId = Lens.lens (externalId :: AnalyticsConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: AnalyticsConfigurationType)
{-# DEPRECATED actExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actRoleARN :: Lens.Lens' AnalyticsConfigurationType (Lude.Maybe Lude.Text)
actRoleARN = Lens.lens (roleARN :: AnalyticsConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: AnalyticsConfigurationType)
{-# DEPRECATED actRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON AnalyticsConfigurationType where
  parseJSON =
    Lude.withObject
      "AnalyticsConfigurationType"
      ( \x ->
          AnalyticsConfigurationType'
            Lude.<$> (x Lude..:? "ApplicationArn")
            Lude.<*> (x Lude..:? "UserDataShared")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "ExternalId")
            Lude.<*> (x Lude..:? "RoleArn")
      )

instance Lude.ToJSON AnalyticsConfigurationType where
  toJSON AnalyticsConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ApplicationArn" Lude..=) Lude.<$> applicationARN,
            ("UserDataShared" Lude..=) Lude.<$> userDataShared,
            ("ApplicationId" Lude..=) Lude.<$> applicationId,
            ("ExternalId" Lude..=) Lude.<$> externalId,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )
