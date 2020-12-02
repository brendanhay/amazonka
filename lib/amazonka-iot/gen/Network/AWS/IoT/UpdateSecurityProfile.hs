{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    updateSecurityProfile,
    UpdateSecurityProfile,

    -- * Request Lenses
    uspAlertTargets,
    uspAdditionalMetricsToRetainV2,
    uspBehaviors,
    uspExpectedVersion,
    uspDeleteAlertTargets,
    uspAdditionalMetricsToRetain,
    uspSecurityProfileDescription,
    uspDeleteBehaviors,
    uspDeleteAdditionalMetricsToRetain,
    uspSecurityProfileName,

    -- * Destructuring the Response
    updateSecurityProfileResponse,
    UpdateSecurityProfileResponse,

    -- * Response Lenses
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSecurityProfile' smart constructor.
data UpdateSecurityProfile = UpdateSecurityProfile'
  { _uspAlertTargets ::
      !(Maybe (Map AlertTargetType (AlertTarget))),
    _uspAdditionalMetricsToRetainV2 ::
      !(Maybe [MetricToRetain]),
    _uspBehaviors :: !(Maybe [Behavior]),
    _uspExpectedVersion :: !(Maybe Integer),
    _uspDeleteAlertTargets :: !(Maybe Bool),
    _uspAdditionalMetricsToRetain ::
      !(Maybe [Text]),
    _uspSecurityProfileDescription :: !(Maybe Text),
    _uspDeleteBehaviors :: !(Maybe Bool),
    _uspDeleteAdditionalMetricsToRetain ::
      !(Maybe Bool),
    _uspSecurityProfileName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uspAlertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- * 'uspAdditionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- * 'uspBehaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- * 'uspExpectedVersion' - The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
--
-- * 'uspDeleteAlertTargets' - If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
--
-- * 'uspAdditionalMetricsToRetain' - /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./  A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- * 'uspSecurityProfileDescription' - A description of the security profile.
--
-- * 'uspDeleteBehaviors' - If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
--
-- * 'uspDeleteAdditionalMetricsToRetain' - If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
--
-- * 'uspSecurityProfileName' - The name of the security profile you want to update.
updateSecurityProfile ::
  -- | 'uspSecurityProfileName'
  Text ->
  UpdateSecurityProfile
updateSecurityProfile pSecurityProfileName_ =
  UpdateSecurityProfile'
    { _uspAlertTargets = Nothing,
      _uspAdditionalMetricsToRetainV2 = Nothing,
      _uspBehaviors = Nothing,
      _uspExpectedVersion = Nothing,
      _uspDeleteAlertTargets = Nothing,
      _uspAdditionalMetricsToRetain = Nothing,
      _uspSecurityProfileDescription = Nothing,
      _uspDeleteBehaviors = Nothing,
      _uspDeleteAdditionalMetricsToRetain = Nothing,
      _uspSecurityProfileName = pSecurityProfileName_
    }

-- | Where the alerts are sent. (Alerts are always sent to the console.)
uspAlertTargets :: Lens' UpdateSecurityProfile (HashMap AlertTargetType (AlertTarget))
uspAlertTargets = lens _uspAlertTargets (\s a -> s {_uspAlertTargets = a}) . _Default . _Map

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
uspAdditionalMetricsToRetainV2 :: Lens' UpdateSecurityProfile [MetricToRetain]
uspAdditionalMetricsToRetainV2 = lens _uspAdditionalMetricsToRetainV2 (\s a -> s {_uspAdditionalMetricsToRetainV2 = a}) . _Default . _Coerce

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
uspBehaviors :: Lens' UpdateSecurityProfile [Behavior]
uspBehaviors = lens _uspBehaviors (\s a -> s {_uspBehaviors = a}) . _Default . _Coerce

-- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
uspExpectedVersion :: Lens' UpdateSecurityProfile (Maybe Integer)
uspExpectedVersion = lens _uspExpectedVersion (\s a -> s {_uspExpectedVersion = a})

-- | If true, delete all @alertTargets@ defined for this security profile. If any @alertTargets@ are defined in the current invocation, an exception occurs.
uspDeleteAlertTargets :: Lens' UpdateSecurityProfile (Maybe Bool)
uspDeleteAlertTargets = lens _uspDeleteAlertTargets (\s a -> s {_uspDeleteAlertTargets = a})

-- | /Please use 'UpdateSecurityProfileRequest$additionalMetricsToRetainV2' instead./  A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
uspAdditionalMetricsToRetain :: Lens' UpdateSecurityProfile [Text]
uspAdditionalMetricsToRetain = lens _uspAdditionalMetricsToRetain (\s a -> s {_uspAdditionalMetricsToRetain = a}) . _Default . _Coerce

-- | A description of the security profile.
uspSecurityProfileDescription :: Lens' UpdateSecurityProfile (Maybe Text)
uspSecurityProfileDescription = lens _uspSecurityProfileDescription (\s a -> s {_uspSecurityProfileDescription = a})

-- | If true, delete all @behaviors@ defined for this security profile. If any @behaviors@ are defined in the current invocation, an exception occurs.
uspDeleteBehaviors :: Lens' UpdateSecurityProfile (Maybe Bool)
uspDeleteBehaviors = lens _uspDeleteBehaviors (\s a -> s {_uspDeleteBehaviors = a})

-- | If true, delete all @additionalMetricsToRetain@ defined for this security profile. If any @additionalMetricsToRetain@ are defined in the current invocation, an exception occurs.
uspDeleteAdditionalMetricsToRetain :: Lens' UpdateSecurityProfile (Maybe Bool)
uspDeleteAdditionalMetricsToRetain = lens _uspDeleteAdditionalMetricsToRetain (\s a -> s {_uspDeleteAdditionalMetricsToRetain = a})

-- | The name of the security profile you want to update.
uspSecurityProfileName :: Lens' UpdateSecurityProfile Text
uspSecurityProfileName = lens _uspSecurityProfileName (\s a -> s {_uspSecurityProfileName = a})

instance AWSRequest UpdateSecurityProfile where
  type Rs UpdateSecurityProfile = UpdateSecurityProfileResponse
  request = patchJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateSecurityProfileResponse'
            <$> (x .?> "alertTargets" .!@ mempty)
            <*> (x .?> "additionalMetricsToRetainV2" .!@ mempty)
            <*> (x .?> "behaviors" .!@ mempty)
            <*> (x .?> "lastModifiedDate")
            <*> (x .?> "version")
            <*> (x .?> "securityProfileName")
            <*> (x .?> "creationDate")
            <*> (x .?> "additionalMetricsToRetain" .!@ mempty)
            <*> (x .?> "securityProfileArn")
            <*> (x .?> "securityProfileDescription")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateSecurityProfile

instance NFData UpdateSecurityProfile

instance ToHeaders UpdateSecurityProfile where
  toHeaders = const mempty

instance ToJSON UpdateSecurityProfile where
  toJSON UpdateSecurityProfile' {..} =
    object
      ( catMaybes
          [ ("alertTargets" .=) <$> _uspAlertTargets,
            ("additionalMetricsToRetainV2" .=)
              <$> _uspAdditionalMetricsToRetainV2,
            ("behaviors" .=) <$> _uspBehaviors,
            ("deleteAlertTargets" .=) <$> _uspDeleteAlertTargets,
            ("additionalMetricsToRetain" .=) <$> _uspAdditionalMetricsToRetain,
            ("securityProfileDescription" .=)
              <$> _uspSecurityProfileDescription,
            ("deleteBehaviors" .=) <$> _uspDeleteBehaviors,
            ("deleteAdditionalMetricsToRetain" .=)
              <$> _uspDeleteAdditionalMetricsToRetain
          ]
      )

instance ToPath UpdateSecurityProfile where
  toPath UpdateSecurityProfile' {..} =
    mconcat ["/security-profiles/", toBS _uspSecurityProfileName]

instance ToQuery UpdateSecurityProfile where
  toQuery UpdateSecurityProfile' {..} =
    mconcat ["expectedVersion" =: _uspExpectedVersion]

-- | /See:/ 'updateSecurityProfileResponse' smart constructor.
data UpdateSecurityProfileResponse = UpdateSecurityProfileResponse'
  { _usprsAlertTargets ::
      !( Maybe
           ( Map
               AlertTargetType
               (AlertTarget)
           )
       ),
    _usprsAdditionalMetricsToRetainV2 ::
      !(Maybe [MetricToRetain]),
    _usprsBehaviors ::
      !(Maybe [Behavior]),
    _usprsLastModifiedDate ::
      !(Maybe POSIX),
    _usprsVersion ::
      !(Maybe Integer),
    _usprsSecurityProfileName ::
      !(Maybe Text),
    _usprsCreationDate ::
      !(Maybe POSIX),
    _usprsAdditionalMetricsToRetain ::
      !(Maybe [Text]),
    _usprsSecurityProfileARN ::
      !(Maybe Text),
    _usprsSecurityProfileDescription ::
      !(Maybe Text),
    _usprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usprsAlertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- * 'usprsAdditionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- * 'usprsBehaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- * 'usprsLastModifiedDate' - The time the security profile was last modified.
--
-- * 'usprsVersion' - The updated version of the security profile.
--
-- * 'usprsSecurityProfileName' - The name of the security profile that was updated.
--
-- * 'usprsCreationDate' - The time the security profile was created.
--
-- * 'usprsAdditionalMetricsToRetain' - /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./  A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- * 'usprsSecurityProfileARN' - The ARN of the security profile that was updated.
--
-- * 'usprsSecurityProfileDescription' - The description of the security profile.
--
-- * 'usprsResponseStatus' - -- | The response status code.
updateSecurityProfileResponse ::
  -- | 'usprsResponseStatus'
  Int ->
  UpdateSecurityProfileResponse
updateSecurityProfileResponse pResponseStatus_ =
  UpdateSecurityProfileResponse'
    { _usprsAlertTargets = Nothing,
      _usprsAdditionalMetricsToRetainV2 = Nothing,
      _usprsBehaviors = Nothing,
      _usprsLastModifiedDate = Nothing,
      _usprsVersion = Nothing,
      _usprsSecurityProfileName = Nothing,
      _usprsCreationDate = Nothing,
      _usprsAdditionalMetricsToRetain = Nothing,
      _usprsSecurityProfileARN = Nothing,
      _usprsSecurityProfileDescription = Nothing,
      _usprsResponseStatus = pResponseStatus_
    }

-- | Where the alerts are sent. (Alerts are always sent to the console.)
usprsAlertTargets :: Lens' UpdateSecurityProfileResponse (HashMap AlertTargetType (AlertTarget))
usprsAlertTargets = lens _usprsAlertTargets (\s a -> s {_usprsAlertTargets = a}) . _Default . _Map

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
usprsAdditionalMetricsToRetainV2 :: Lens' UpdateSecurityProfileResponse [MetricToRetain]
usprsAdditionalMetricsToRetainV2 = lens _usprsAdditionalMetricsToRetainV2 (\s a -> s {_usprsAdditionalMetricsToRetainV2 = a}) . _Default . _Coerce

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
usprsBehaviors :: Lens' UpdateSecurityProfileResponse [Behavior]
usprsBehaviors = lens _usprsBehaviors (\s a -> s {_usprsBehaviors = a}) . _Default . _Coerce

-- | The time the security profile was last modified.
usprsLastModifiedDate :: Lens' UpdateSecurityProfileResponse (Maybe UTCTime)
usprsLastModifiedDate = lens _usprsLastModifiedDate (\s a -> s {_usprsLastModifiedDate = a}) . mapping _Time

-- | The updated version of the security profile.
usprsVersion :: Lens' UpdateSecurityProfileResponse (Maybe Integer)
usprsVersion = lens _usprsVersion (\s a -> s {_usprsVersion = a})

-- | The name of the security profile that was updated.
usprsSecurityProfileName :: Lens' UpdateSecurityProfileResponse (Maybe Text)
usprsSecurityProfileName = lens _usprsSecurityProfileName (\s a -> s {_usprsSecurityProfileName = a})

-- | The time the security profile was created.
usprsCreationDate :: Lens' UpdateSecurityProfileResponse (Maybe UTCTime)
usprsCreationDate = lens _usprsCreationDate (\s a -> s {_usprsCreationDate = a}) . mapping _Time

-- | /Please use 'UpdateSecurityProfileResponse$additionalMetricsToRetainV2' instead./  A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the security profile's @behaviors@ , but it is also retained for any metric specified here.
usprsAdditionalMetricsToRetain :: Lens' UpdateSecurityProfileResponse [Text]
usprsAdditionalMetricsToRetain = lens _usprsAdditionalMetricsToRetain (\s a -> s {_usprsAdditionalMetricsToRetain = a}) . _Default . _Coerce

-- | The ARN of the security profile that was updated.
usprsSecurityProfileARN :: Lens' UpdateSecurityProfileResponse (Maybe Text)
usprsSecurityProfileARN = lens _usprsSecurityProfileARN (\s a -> s {_usprsSecurityProfileARN = a})

-- | The description of the security profile.
usprsSecurityProfileDescription :: Lens' UpdateSecurityProfileResponse (Maybe Text)
usprsSecurityProfileDescription = lens _usprsSecurityProfileDescription (\s a -> s {_usprsSecurityProfileDescription = a})

-- | -- | The response status code.
usprsResponseStatus :: Lens' UpdateSecurityProfileResponse Int
usprsResponseStatus = lens _usprsResponseStatus (\s a -> s {_usprsResponseStatus = a})

instance NFData UpdateSecurityProfileResponse
