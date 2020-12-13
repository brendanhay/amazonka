{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender security profile.
module Network.AWS.IoT.UpdateSecurityProfile
  ( -- * Creating a request
    UpdateSecurityProfile (..),
    mkUpdateSecurityProfile,

    -- ** Request lenses
    uspAlertTargets,
    uspAdditionalMetricsToRetainV2,
    uspBehaviors,
    uspExpectedVersion,
    uspSecurityProfileName,
    uspDeleteAlertTargets,
    uspAdditionalMetricsToRetain,
    uspSecurityProfileDescription,
    uspDeleteBehaviors,
    uspDeleteAdditionalMetricsToRetain,

    -- * Destructuring the response
    UpdateSecurityProfileResponse (..),
    mkUpdateSecurityProfileResponse,

    -- ** Response lenses
    usprsAlertTargets,
    usprsAdditionalMetricsToRetainV2,
    usprsBehaviors,
    usprsLastModifiedDate,
    usprsVersion,
    usprsSecurityProfileName,
    usprsCreationDate,
    usprsAdditionalMetricsToRetain,
    usprsSecurityProfileARN,
    usprsSecurityProfileDescription,
    usprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSecurityProfile' smart constructor.
data UpdateSecurityProfile = UpdateSecurityProfile'
  { -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)),
    -- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
    additionalMetricsToRetainV2 :: Lude.Maybe [MetricToRetain],
    -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: Lude.Maybe [Behavior],
    -- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The name of the security profile you want to update.
    securityProfileName :: Lude.Text,
    -- | If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
    deleteAlertTargets :: Lude.Maybe Lude.Bool,
    -- | /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetain :: Lude.Maybe [Lude.Text],
    -- | A description of the security profile.
    securityProfileDescription :: Lude.Maybe Lude.Text,
    -- | If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
    deleteBehaviors :: Lude.Maybe Lude.Bool,
    -- | If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
    deleteAdditionalMetricsToRetain :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecurityProfile' with the minimum fields required to make a request.
--
-- * 'alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
-- * 'additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
-- * 'behaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
-- * 'expectedVersion' - The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
-- * 'securityProfileName' - The name of the security profile you want to update.
-- * 'deleteAlertTargets' - If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
-- * 'additionalMetricsToRetain' - /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
-- * 'securityProfileDescription' - A description of the security profile.
-- * 'deleteBehaviors' - If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
-- * 'deleteAdditionalMetricsToRetain' - If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
mkUpdateSecurityProfile ::
  -- | 'securityProfileName'
  Lude.Text ->
  UpdateSecurityProfile
mkUpdateSecurityProfile pSecurityProfileName_ =
  UpdateSecurityProfile'
    { alertTargets = Lude.Nothing,
      additionalMetricsToRetainV2 = Lude.Nothing,
      behaviors = Lude.Nothing,
      expectedVersion = Lude.Nothing,
      securityProfileName = pSecurityProfileName_,
      deleteAlertTargets = Lude.Nothing,
      additionalMetricsToRetain = Lude.Nothing,
      securityProfileDescription = Lude.Nothing,
      deleteBehaviors = Lude.Nothing,
      deleteAdditionalMetricsToRetain = Lude.Nothing
    }

-- | Where the alerts are sent. (Alerts are always sent to the console.)
--
-- /Note:/ Consider using 'alertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspAlertTargets :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)))
uspAlertTargets = Lens.lens (alertTargets :: UpdateSecurityProfile -> Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget))) (\s a -> s {alertTargets = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspAdditionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe [MetricToRetain])
uspAdditionalMetricsToRetainV2 = Lens.lens (additionalMetricsToRetainV2 :: UpdateSecurityProfile -> Lude.Maybe [MetricToRetain]) (\s a -> s {additionalMetricsToRetainV2 = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspBehaviors :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe [Behavior])
uspBehaviors = Lens.lens (behaviors :: UpdateSecurityProfile -> Lude.Maybe [Behavior]) (\s a -> s {behaviors = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspExpectedVersion :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe Lude.Integer)
uspExpectedVersion = Lens.lens (expectedVersion :: UpdateSecurityProfile -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the security profile you want to update.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspSecurityProfileName :: Lens.Lens' UpdateSecurityProfile Lude.Text
uspSecurityProfileName = Lens.lens (securityProfileName :: UpdateSecurityProfile -> Lude.Text) (\s a -> s {securityProfileName = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
--
-- /Note:/ Consider using 'deleteAlertTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDeleteAlertTargets :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe Lude.Bool)
uspDeleteAlertTargets = Lens.lens (deleteAlertTargets :: UpdateSecurityProfile -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAlertTargets = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspDeleteAlertTargets "Use generic-lens or generic-optics with 'deleteAlertTargets' instead." #-}

-- | /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe [Lude.Text])
uspAdditionalMetricsToRetain = Lens.lens (additionalMetricsToRetain :: UpdateSecurityProfile -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMetricsToRetain = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | A description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspSecurityProfileDescription :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe Lude.Text)
uspSecurityProfileDescription = Lens.lens (securityProfileDescription :: UpdateSecurityProfile -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileDescription = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
--
-- /Note:/ Consider using 'deleteBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDeleteBehaviors :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe Lude.Bool)
uspDeleteBehaviors = Lens.lens (deleteBehaviors :: UpdateSecurityProfile -> Lude.Maybe Lude.Bool) (\s a -> s {deleteBehaviors = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspDeleteBehaviors "Use generic-lens or generic-optics with 'deleteBehaviors' instead." #-}

-- | If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
--
-- /Note:/ Consider using 'deleteAdditionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDeleteAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfile (Lude.Maybe Lude.Bool)
uspDeleteAdditionalMetricsToRetain = Lens.lens (deleteAdditionalMetricsToRetain :: UpdateSecurityProfile -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAdditionalMetricsToRetain = a} :: UpdateSecurityProfile)
{-# DEPRECATED uspDeleteAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'deleteAdditionalMetricsToRetain' instead." #-}

instance Lude.AWSRequest UpdateSecurityProfile where
  type Rs UpdateSecurityProfile = UpdateSecurityProfileResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSecurityProfileResponse'
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

instance Lude.ToHeaders UpdateSecurityProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateSecurityProfile where
  toJSON UpdateSecurityProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("alertTargets" Lude..=) Lude.<$> alertTargets,
            ("additionalMetricsToRetainV2" Lude..=)
              Lude.<$> additionalMetricsToRetainV2,
            ("behaviors" Lude..=) Lude.<$> behaviors,
            ("deleteAlertTargets" Lude..=) Lude.<$> deleteAlertTargets,
            ("additionalMetricsToRetain" Lude..=)
              Lude.<$> additionalMetricsToRetain,
            ("securityProfileDescription" Lude..=)
              Lude.<$> securityProfileDescription,
            ("deleteBehaviors" Lude..=) Lude.<$> deleteBehaviors,
            ("deleteAdditionalMetricsToRetain" Lude..=)
              Lude.<$> deleteAdditionalMetricsToRetain
          ]
      )

instance Lude.ToPath UpdateSecurityProfile where
  toPath UpdateSecurityProfile' {..} =
    Lude.mconcat
      ["/security-profiles/", Lude.toBS securityProfileName]

instance Lude.ToQuery UpdateSecurityProfile where
  toQuery UpdateSecurityProfile' {..} =
    Lude.mconcat ["expectedVersion" Lude.=: expectedVersion]

-- | /See:/ 'mkUpdateSecurityProfileResponse' smart constructor.
data UpdateSecurityProfileResponse = UpdateSecurityProfileResponse'
  { -- | Where the alerts are sent. (Alerts are always sent to the console.)
    alertTargets :: Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)),
    -- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
    additionalMetricsToRetainV2 :: Lude.Maybe [MetricToRetain],
    -- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
    behaviors :: Lude.Maybe [Behavior],
    -- | The time the security profile was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The updated version of the security profile.
    version :: Lude.Maybe Lude.Integer,
    -- | The name of the security profile that was updated.
    securityProfileName :: Lude.Maybe Lude.Text,
    -- | The time the security profile was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./
    --
    -- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
    additionalMetricsToRetain :: Lude.Maybe [Lude.Text],
    -- | The ARN of the security profile that was updated.
    securityProfileARN :: Lude.Maybe Lude.Text,
    -- | The description of the security profile.
    securityProfileDescription :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecurityProfileResponse' with the minimum fields required to make a request.
--
-- * 'alertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
-- * 'additionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
-- * 'behaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
-- * 'lastModifiedDate' - The time the security profile was last modified.
-- * 'version' - The updated version of the security profile.
-- * 'securityProfileName' - The name of the security profile that was updated.
-- * 'creationDate' - The time the security profile was created.
-- * 'additionalMetricsToRetain' - /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
-- * 'securityProfileARN' - The ARN of the security profile that was updated.
-- * 'securityProfileDescription' - The description of the security profile.
-- * 'responseStatus' - The response status code.
mkUpdateSecurityProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSecurityProfileResponse
mkUpdateSecurityProfileResponse pResponseStatus_ =
  UpdateSecurityProfileResponse'
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
usprsAlertTargets :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget)))
usprsAlertTargets = Lens.lens (alertTargets :: UpdateSecurityProfileResponse -> Lude.Maybe (Lude.HashMap AlertTargetType (AlertTarget))) (\s a -> s {alertTargets = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsAlertTargets "Use generic-lens or generic-optics with 'alertTargets' instead." #-}

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetainV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsAdditionalMetricsToRetainV2 :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe [MetricToRetain])
usprsAdditionalMetricsToRetainV2 = Lens.lens (additionalMetricsToRetainV2 :: UpdateSecurityProfileResponse -> Lude.Maybe [MetricToRetain]) (\s a -> s {additionalMetricsToRetainV2 = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsAdditionalMetricsToRetainV2 "Use generic-lens or generic-optics with 'additionalMetricsToRetainV2' instead." #-}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsBehaviors :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe [Behavior])
usprsBehaviors = Lens.lens (behaviors :: UpdateSecurityProfileResponse -> Lude.Maybe [Behavior]) (\s a -> s {behaviors = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsBehaviors "Use generic-lens or generic-optics with 'behaviors' instead." #-}

-- | The time the security profile was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsLastModifiedDate :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe Lude.Timestamp)
usprsLastModifiedDate = Lens.lens (lastModifiedDate :: UpdateSecurityProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The updated version of the security profile.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsVersion :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe Lude.Integer)
usprsVersion = Lens.lens (version :: UpdateSecurityProfileResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the security profile that was updated.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsSecurityProfileName :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe Lude.Text)
usprsSecurityProfileName = Lens.lens (securityProfileName :: UpdateSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileName = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | The time the security profile was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsCreationDate :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe Lude.Timestamp)
usprsCreationDate = Lens.lens (creationDate :: UpdateSecurityProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./
--
-- A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- /Note:/ Consider using 'additionalMetricsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsAdditionalMetricsToRetain :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe [Lude.Text])
usprsAdditionalMetricsToRetain = Lens.lens (additionalMetricsToRetain :: UpdateSecurityProfileResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalMetricsToRetain = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsAdditionalMetricsToRetain "Use generic-lens or generic-optics with 'additionalMetricsToRetain' instead." #-}

-- | The ARN of the security profile that was updated.
--
-- /Note:/ Consider using 'securityProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsSecurityProfileARN :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe Lude.Text)
usprsSecurityProfileARN = Lens.lens (securityProfileARN :: UpdateSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileARN = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsSecurityProfileARN "Use generic-lens or generic-optics with 'securityProfileARN' instead." #-}

-- | The description of the security profile.
--
-- /Note:/ Consider using 'securityProfileDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsSecurityProfileDescription :: Lens.Lens' UpdateSecurityProfileResponse (Lude.Maybe Lude.Text)
usprsSecurityProfileDescription = Lens.lens (securityProfileDescription :: UpdateSecurityProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {securityProfileDescription = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsSecurityProfileDescription "Use generic-lens or generic-optics with 'securityProfileDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsResponseStatus :: Lens.Lens' UpdateSecurityProfileResponse Lude.Int
usprsResponseStatus = Lens.lens (responseStatus :: UpdateSecurityProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSecurityProfileResponse)
{-# DEPRECATED usprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
