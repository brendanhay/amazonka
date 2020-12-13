{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender security profile.
module Network.AWS.IoT.DescribeSecurityProfile
  ( -- * Creating a request
    DescribeSecurityProfile (..),
    mkDescribeSecurityProfile,

    -- ** Request lenses
    dSecurityProfileName,

    -- * Destructuring the response
    DescribeSecurityProfileResponse (..),
    mkDescribeSecurityProfileResponse,

    -- ** Response lenses
    dspfrsAlertTargets,
    dspfrsAdditionalMetricsToRetainV2,
    dspfrsBehaviors,
    dspfrsLastModifiedDate,
    dspfrsVersion,
    dspfrsSecurityProfileName,
    dspfrsCreationDate,
    dspfrsAdditionalMetricsToRetain,
    dspfrsSecurityProfileARN,
    dspfrsSecurityProfileDescription,
    dspfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSecurityProfile' smart constructor.
newtype DescribeSecurityProfile = DescribeSecurityProfile'
  { -- | The name of the security profile whose information you want to get.
    securityProfileName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityProfile' with the minimum fields required to make a request.
--
-- * 'securityProfileName' - The name of the security profile whose information you want to get.
mkDescribeSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  DescribeSecurityProfile
mkDescribeSecurityProfile pSecurityProfileName_ =
  DescribeSecurityProfile'
    { securityProfileName =
        pSecurityProfileName_
    }

-- | The name of the security profile whose information you want to get.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSecurityProfileName :: Lens.Lens' DescribeSecurityProfile Lude.Text
dSecurityProfileName = Lens.lens (securityProfileName :: DescribeSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: DescribeSecurityProfile)
{-# DEPRECATED dSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

instance Lude.AWSRequest DescribeSecurityProfile where
  type Rs DescribeSecurityProfile = DescribeSecurityProfileResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSecurityProfileResponse'
            Lude.<$> (x Lude..?> "alertTargets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "additionalMetricsToRetainV2" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "behaviors" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "securityProfileName")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "additionalMetricsToRetain" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "securityProfileArn")
            Lude.<*> (x Lude..?> "securityProfileDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSecurityProfile where
  toPath DescribeSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName]

instance Lude.ToQuery DescribeSecurityProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'
  { -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)),
    -- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
    additionalMetricsToRetainV2 :: Lude.Maybe [MetricToRetain],
    -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: Lude.Maybe [Behavior],
    -- | The time the security profile was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The version of the security profile. A new version is generated whenever the security profile is updated.
    version :: Lude.Maybe Lude.Integer,
    -- | The name of the security profile.
    securityProfileName :: Lude.Maybe Lude.Text,
    -- | The time the security profile was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetain :: Lude.Maybe [Lude.Text],
    -- | The ARN of the security profile.
    securityProfileARN :: Lude.Maybe Lude.Text,
    -- | A description of the security profile (associated with the security profile when it was created or updated).
    securityProfileDescription :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
-- * 'additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
-- * 'behaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
-- * 'lastModifiedDate' - The time the security profile was last modified.
-- * 'version' - The version of the security profile. A new version is generated whenever the security profile is updated.
-- * 'securityProfileName' - The name of the security profile.
-- * 'creationDate' - The time the security profile was created.
-- * 'additionalMetricsToRetain' - /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
-- * 'securityProfileARN' - The ARN of the security profile.
-- * 'securityProfileDescription' - A description of the security profile (associated with the security profile when it was created or updated).
-- * 'responseStatus' - The response status code.
mkDescribeSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSecurityProfileResponse
mkDescribeSecurityProfileResponse pResponseStatus_ =
  DescribeSecurityProfileResponse'
    { alertTargets = Lude.Nothing,
      additionalMetricsToRetainV2 = Lude.Nothing,
      behaviors = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      version = Lude.Nothing,
      securityProfileName = Lude.Nothing,
      creationDate = Lude.Nothing,
      additionalMetricsToRetain = Lude.Nothing,
      securityProfileARN = Lude.Nothing,
      securityProfileDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Where the alerts are sent. (Alerts are always sent to the console.)
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsAlertTargets :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)))
dspfrsAlertTargets = Lens.lens (alertTargets :: DescribeSecurityProfileResponse -> Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget))) (\s a -> s {alertTargets = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsAdditionalMetricsToRetainV2 :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe [MetricToRetain])
dspfrsAdditionalMetricsToRetainV2 = Lens.lens (additionalMetricsToRetainV2 :: DescribeSecurityProfileResponse -> Lude.Maybe [MetricToRetain]) (\s a -> s {additionalMetricsToRetainV2 = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsBehaviors :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe [Behavior])
dspfrsBehaviors = Lens.lens (behaviors :: DescribeSecurityProfileResponse -> Lude.Maybe [Behavior]) (\s a -> s {behaviors = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | The time the security profile was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsLastModifiedDate :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Timestamp)
dspfrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The version of the security profile. A new version is generated whenever the security profile is updated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsVersion :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Integer)
dspfrsVersion = Lens.lens (version :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsSecurityProfileName :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Text)
dspfrsSecurityProfileName = Lens.lens (securityProfileName :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The time the security profile was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsCreationDate :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Timestamp)
dspfrsCreationDate = Lens.lens (creationDate :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsAdditionalMetricsToRetain :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe [Lude.Text])
dspfrsAdditionalMetricsToRetain = Lens.lens (additionalMetricsToRetain :: DescribeSecurityProfileResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMetricsToRetain = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'securityProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsSecurityProfileARN :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Text)
dspfrsSecurityProfileARN = Lens.lens (securityProfileARN :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileARN = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsSecurityProfileARN "Use generic-lens or generic-optics with 'securityProfileARN' instead." #-}

-- | A description of the security profile (associated with the security profile when it was created or updated).
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsSecurityProfileDescription :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Text)
dspfrsSecurityProfileDescription = Lens.lens (securityProfileDescription :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileDescription = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfrsResponseStatus :: Lens.Lens' DescribeSecurityProfileResponse Lude.Int
dspfrsResponseStatus = Lens.lens (responseStatus :: DescribeSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
