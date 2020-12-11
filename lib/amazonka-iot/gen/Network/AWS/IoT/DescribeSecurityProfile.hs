{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dspsrsAlertTargets,
    dspsrsAdditionalMetricsToRetainV2,
    dspsrsBehaviors,
    dspsrsLastModifiedDate,
    dspsrsVersion,
    dspsrsSecurityProfileName,
    dspsrsCreationDate,
    dspsrsAdditionalMetricsToRetain,
    dspsrsSecurityProfileARN,
    dspsrsSecurityProfileDescription,
    dspsrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSecurityProfile' smart constructor.
newtype DescribeSecurityProfile = DescribeSecurityProfile'
  { securityProfileName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  { alertTargets ::
      Lude.Maybe
        ( Lude.HashMap
            AlertTargetType
            (AlertTarget)
        ),
    additionalMetricsToRetainV2 ::
      Lude.Maybe [MetricToRetain],
    behaviors ::
      Lude.Maybe [Behavior],
    lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    version ::
      Lude.Maybe Lude.Integer,
    securityProfileName ::
      Lude.Maybe Lude.Text,
    creationDate ::
      Lude.Maybe Lude.Timestamp,
    additionalMetricsToRetain ::
      Lude.Maybe [Lude.Text],
    securityProfileARN ::
      Lude.Maybe Lude.Text,
    securityProfileDescription ::
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

-- | Creates a value of 'DescribeSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'additionalMetricsToRetain' - /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
-- * 'additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
-- * 'alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
-- * 'behaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
-- * 'creationDate' - The time the security profile was created.
-- * 'lastModifiedDate' - The time the security profile was last modified.
-- * 'responseStatus' - The response status code.
-- * 'securityProfileARN' - The ARN of the security profile.
-- * 'securityProfileDescription' - A description of the security profile (associated with the security profile when it was created or updated).
-- * 'securityProfileName' - The name of the security profile.
-- * 'version' - The version of the security profile. A new version is generated whenever the security profile is updated.
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
dspsrsAlertTargets :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)))
dspsrsAlertTargets = Lens.lens (alertTargets :: DescribeSecurityProfileResponse -> Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget))) (\s a -> s {alertTargets = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsAdditionalMetricsToRetainV2 :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe [MetricToRetain])
dspsrsAdditionalMetricsToRetainV2 = Lens.lens (additionalMetricsToRetainV2 :: DescribeSecurityProfileResponse -> Lude.Maybe [MetricToRetain]) (\s a -> s {additionalMetricsToRetainV2 = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsBehaviors :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe [Behavior])
dspsrsBehaviors = Lens.lens (behaviors :: DescribeSecurityProfileResponse -> Lude.Maybe [Behavior]) (\s a -> s {behaviors = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | The time the security profile was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsLastModifiedDate :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Timestamp)
dspsrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The version of the security profile. A new version is generated whenever the security profile is updated.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsVersion :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Integer)
dspsrsVersion = Lens.lens (version :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsSecurityProfileName :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Text)
dspsrsSecurityProfileName = Lens.lens (securityProfileName :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The time the security profile was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsCreationDate :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Timestamp)
dspsrsCreationDate = Lens.lens (creationDate :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsAdditionalMetricsToRetain :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe [Lude.Text])
dspsrsAdditionalMetricsToRetain = Lens.lens (additionalMetricsToRetain :: DescribeSecurityProfileResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMetricsToRetain = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'securityProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsSecurityProfileARN :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Text)
dspsrsSecurityProfileARN = Lens.lens (securityProfileARN :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileARN = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsSecurityProfileARN "Use generic-lens or generic-optics with 'securityProfileARN' instead." #-}

-- | A description of the security profile (associated with the security profile when it was created or updated).
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsSecurityProfileDescription :: Lens.Lens' DescribeSecurityProfileResponse (Lude.Maybe Lude.Text)
dspsrsSecurityProfileDescription = Lens.lens (securityProfileDescription :: DescribeSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileDescription = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsResponseStatus :: Lens.Lens' DescribeSecurityProfileResponse Lude.Int
dspsrsResponseStatus = Lens.lens (responseStatus :: DescribeSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSecurityProfileResponse)
{-# DEPRECATED dspsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
