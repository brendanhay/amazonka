{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Device Defender security profile.
module Network.AWS.IoT.CreateSecurityProfile
  ( -- * Creating a request
    CreateSecurityProfile (..),
    mkCreateSecurityProfile,

    -- ** Request lenses
    cspAlertTargets,
    cspAdditionalMetricsToRetainV2,
    cspBehaviors,
    cspAdditionalMetricsToRetain,
    cspSecurityProfileDescription,
    cspTags,
    cspSecurityProfileName,

    -- * Destructuring the response
    CreateSecurityProfileResponse (..),
    mkCreateSecurityProfileResponse,

    -- ** Response lenses
    csprsSecurityProfileName,
    csprsSecurityProfileARN,
    csprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSecurityProfile' smart constructor.
data CreateSecurityProfile = CreateSecurityProfile'
  { alertTargets ::
      Lude.Maybe
        (Lude.HashMap AlertTargetType (AlertTarget)),
    additionalMetricsToRetainV2 ::
      Lude.Maybe [MetricToRetain],
    behaviors :: Lude.Maybe [Behavior],
    additionalMetricsToRetain ::
      Lude.Maybe [Lude.Text],
    securityProfileDescription ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    securityProfileName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityProfile' with the minimum fields required to make a request.
--
-- * 'additionalMetricsToRetain' - /Please use 'CreateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
-- * 'additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
-- * 'alertTargets' - Specifies the destinations to which alerts are sent. (Alerts are always sent to the console.) Alerts are generated when a device (thing) violates a behavior.
-- * 'behaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
-- * 'securityProfileDescription' - A description of the security profile.
-- * 'securityProfileName' - The name you are giving to the security profile.
-- * 'tags' - Metadata that can be used to manage the security profile.
mkCreateSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  CreateSecurityProfile
mkCreateSecurityProfile pSecurityProfileName_ =
  CreateSecurityProfile'
    { alertTargets = Lude.Nothing,
      additionalMetricsToRetainV2 = Lude.Nothing,
      behaviors = Lude.Nothing,
      additionalMetricsToRetain = Lude.Nothing,
      securityProfileDescription = Lude.Nothing,
      tags = Lude.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | Specifies the destinations to which alerts are sent. (Alerts are always sent to the console.) Alerts are generated when a device (thing) violates a behavior.
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAlertTargets :: Lens.Lens' CreateSecurityProfile (Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)))
cspAlertTargets = Lens.lens (alertTargets :: CreateSecurityProfile -> Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget))) (\s a -> s {alertTargets = a} :: CreateSecurityProfile)
{-# DEPRECATED cspAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAdditionalMetricsToRetainV2 :: Lens.Lens' CreateSecurityProfile (Lude.Maybe [MetricToRetain])
cspAdditionalMetricsToRetainV2 = Lens.lens (additionalMetricsToRetainV2 :: CreateSecurityProfile -> Lude.Maybe [MetricToRetain]) (\s a -> s {additionalMetricsToRetainV2 = a} :: CreateSecurityProfile)
{-# DEPRECATED cspAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspBehaviors :: Lens.Lens' CreateSecurityProfile (Lude.Maybe [Behavior])
cspBehaviors = Lens.lens (behaviors :: CreateSecurityProfile -> Lude.Maybe [Behavior]) (\s a -> s {behaviors = a} :: CreateSecurityProfile)
{-# DEPRECATED cspBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | /Please use 'CreateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspAdditionalMetricsToRetain :: Lens.Lens' CreateSecurityProfile (Lude.Maybe [Lude.Text])
cspAdditionalMetricsToRetain = Lens.lens (additionalMetricsToRetain :: CreateSecurityProfile -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMetricsToRetain = a} :: CreateSecurityProfile)
{-# DEPRECATED cspAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | A description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSecurityProfileDescription :: Lens.Lens' CreateSecurityProfile (Lude.Maybe Lude.Text)
cspSecurityProfileDescription = Lens.lens (securityProfileDescription :: CreateSecurityProfile -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileDescription = a} :: CreateSecurityProfile)
{-# DEPRECATED cspSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | Metadata that can be used to manage the security profile.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspTags :: Lens.Lens' CreateSecurityProfile (Lude.Maybe [Tag])
cspTags = Lens.lens (tags :: CreateSecurityProfile -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSecurityProfile)
{-# DEPRECATED cspTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name you are giving to the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspSecurityProfileName :: Lens.Lens' CreateSecurityProfile Lude.Text
cspSecurityProfileName = Lens.lens (securityProfileName :: CreateSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: CreateSecurityProfile)
{-# DEPRECATED cspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

instance Lude.AWSRequest CreateSecurityProfile where
  type Rs CreateSecurityProfile = CreateSecurityProfileResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSecurityProfileResponse'
            Lude.<$> (x Lude..?> "securityProfileName")
            Lude.<*> (x Lude..?> "securityProfileArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateSecurityProfile where
  toJSON CreateSecurityProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("alertTargets" Lude..=) Lude.<$> alertTargets,
            ("additionalMetricsToRetainV2" Lude..=)
              Lude.<$> additionalMetricsToRetainV2,
            ("behaviors" Lude..=) Lude.<$> behaviors,
            ("additionalMetricsToRetain" Lude..=)
              Lude.<$> additionalMetricsToRetain,
            ("securityProfileDescription" Lude..=)
              Lude.<$> securityProfileDescription,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateSecurityProfile where
  toPath CreateSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName]

instance Lude.ToQuery CreateSecurityProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSecurityProfileResponse' smart constructor.
data CreateSecurityProfileResponse = CreateSecurityProfileResponse'
  { securityProfileName ::
      Lude.Maybe Lude.Text,
    securityProfileARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'securityProfileARN' - The ARN of the security profile.
-- * 'securityProfileName' - The name you gave to the security profile.
mkCreateSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSecurityProfileResponse
mkCreateSecurityProfileResponse pResponseStatus_ =
  CreateSecurityProfileResponse'
    { securityProfileName =
        Lude.Nothing,
      securityProfileARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name you gave to the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsSecurityProfileName :: Lens.Lens' CreateSecurityProfileResponse (Lude.Maybe Lude.Text)
csprsSecurityProfileName = Lens.lens (securityProfileName :: CreateSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: CreateSecurityProfileResponse)
{-# DEPRECATED csprsSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'securityProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsSecurityProfileARN :: Lens.Lens' CreateSecurityProfileResponse (Lude.Maybe Lude.Text)
csprsSecurityProfileARN = Lens.lens (securityProfileARN :: CreateSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileARN = a} :: CreateSecurityProfileResponse)
{-# DEPRECATED csprsSecurityProfileARN "Use generic-lens or generic-optics with 'securityProfileARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csprsResponseStatus :: Lens.Lens' CreateSecurityProfileResponse Lude.Int
csprsResponseStatus = Lens.lens (responseStatus :: CreateSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSecurityProfileResponse)
{-# DEPRECATED csprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
