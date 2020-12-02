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
-- Module      : Network.AWS.IoT.DescribeSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender security profile.
module Network.AWS.IoT.DescribeSecurityProfile
  ( -- * Creating a Request
    describeSecurityProfile,
    DescribeSecurityProfile,

    -- * Request Lenses
    dSecurityProfileName,

    -- * Destructuring the Response
    describeSecurityProfileResponse,
    DescribeSecurityProfileResponse,

    -- * Response Lenses
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSecurityProfile' smart constructor.
newtype DescribeSecurityProfile = DescribeSecurityProfile'
  { _dSecurityProfileName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSecurityProfileName' - The name of the security profile whose information you want to get.
describeSecurityProfile ::
  -- | 'dSecurityProfileName'
  Text ->
  DescribeSecurityProfile
describeSecurityProfile pSecurityProfileName_ =
  DescribeSecurityProfile'
    { _dSecurityProfileName =
        pSecurityProfileName_
    }

-- | The name of the security profile whose information you want to get.
dSecurityProfileName :: Lens' DescribeSecurityProfile Text
dSecurityProfileName = lens _dSecurityProfileName (\s a -> s {_dSecurityProfileName = a})

instance AWSRequest DescribeSecurityProfile where
  type Rs DescribeSecurityProfile = DescribeSecurityProfileResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeSecurityProfileResponse'
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

instance Hashable DescribeSecurityProfile

instance NFData DescribeSecurityProfile

instance ToHeaders DescribeSecurityProfile where
  toHeaders = const mempty

instance ToPath DescribeSecurityProfile where
  toPath DescribeSecurityProfile' {..} =
    mconcat ["/security-profiles/", toBS _dSecurityProfileName]

instance ToQuery DescribeSecurityProfile where
  toQuery = const mempty

-- | /See:/ 'describeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'
  { _dspsrsAlertTargets ::
      !( Maybe
           ( Map
               AlertTargetType
               (AlertTarget)
           )
       ),
    _dspsrsAdditionalMetricsToRetainV2 ::
      !(Maybe [MetricToRetain]),
    _dspsrsBehaviors ::
      !(Maybe [Behavior]),
    _dspsrsLastModifiedDate ::
      !(Maybe POSIX),
    _dspsrsVersion ::
      !(Maybe Integer),
    _dspsrsSecurityProfileName ::
      !(Maybe Text),
    _dspsrsCreationDate ::
      !(Maybe POSIX),
    _dspsrsAdditionalMetricsToRetain ::
      !(Maybe [Text]),
    _dspsrsSecurityProfileARN ::
      !(Maybe Text),
    _dspsrsSecurityProfileDescription ::
      !(Maybe Text),
    _dspsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspsrsAlertTargets' - Where the alerts are sent. (Alerts are always sent to the console.)
--
-- * 'dspsrsAdditionalMetricsToRetainV2' - A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
--
-- * 'dspsrsBehaviors' - Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- * 'dspsrsLastModifiedDate' - The time the security profile was last modified.
--
-- * 'dspsrsVersion' - The version of the security profile. A new version is generated whenever the security profile is updated.
--
-- * 'dspsrsSecurityProfileName' - The name of the security profile.
--
-- * 'dspsrsCreationDate' - The time the security profile was created.
--
-- * 'dspsrsAdditionalMetricsToRetain' - /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./  A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
--
-- * 'dspsrsSecurityProfileARN' - The ARN of the security profile.
--
-- * 'dspsrsSecurityProfileDescription' - A description of the security profile (associated with the security profile when it was created or updated).
--
-- * 'dspsrsResponseStatus' - -- | The response status code.
describeSecurityProfileResponse ::
  -- | 'dspsrsResponseStatus'
  Int ->
  DescribeSecurityProfileResponse
describeSecurityProfileResponse pResponseStatus_ =
  DescribeSecurityProfileResponse'
    { _dspsrsAlertTargets = Nothing,
      _dspsrsAdditionalMetricsToRetainV2 = Nothing,
      _dspsrsBehaviors = Nothing,
      _dspsrsLastModifiedDate = Nothing,
      _dspsrsVersion = Nothing,
      _dspsrsSecurityProfileName = Nothing,
      _dspsrsCreationDate = Nothing,
      _dspsrsAdditionalMetricsToRetain = Nothing,
      _dspsrsSecurityProfileARN = Nothing,
      _dspsrsSecurityProfileDescription = Nothing,
      _dspsrsResponseStatus = pResponseStatus_
    }

-- | Where the alerts are sent. (Alerts are always sent to the console.)
dspsrsAlertTargets :: Lens' DescribeSecurityProfileResponse (HashMap AlertTargetType (AlertTarget))
dspsrsAlertTargets = lens _dspsrsAlertTargets (\s a -> s {_dspsrsAlertTargets = a}) . _Default . _Map

-- | A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's behaviors, but it is also retained for any metric specified here.
dspsrsAdditionalMetricsToRetainV2 :: Lens' DescribeSecurityProfileResponse [MetricToRetain]
dspsrsAdditionalMetricsToRetainV2 = lens _dspsrsAdditionalMetricsToRetainV2 (\s a -> s {_dspsrsAdditionalMetricsToRetainV2 = a}) . _Default . _Coerce

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
dspsrsBehaviors :: Lens' DescribeSecurityProfileResponse [Behavior]
dspsrsBehaviors = lens _dspsrsBehaviors (\s a -> s {_dspsrsBehaviors = a}) . _Default . _Coerce

-- | The time the security profile was last modified.
dspsrsLastModifiedDate :: Lens' DescribeSecurityProfileResponse (Maybe UTCTime)
dspsrsLastModifiedDate = lens _dspsrsLastModifiedDate (\s a -> s {_dspsrsLastModifiedDate = a}) . mapping _Time

-- | The version of the security profile. A new version is generated whenever the security profile is updated.
dspsrsVersion :: Lens' DescribeSecurityProfileResponse (Maybe Integer)
dspsrsVersion = lens _dspsrsVersion (\s a -> s {_dspsrsVersion = a})

-- | The name of the security profile.
dspsrsSecurityProfileName :: Lens' DescribeSecurityProfileResponse (Maybe Text)
dspsrsSecurityProfileName = lens _dspsrsSecurityProfileName (\s a -> s {_dspsrsSecurityProfileName = a})

-- | The time the security profile was created.
dspsrsCreationDate :: Lens' DescribeSecurityProfileResponse (Maybe UTCTime)
dspsrsCreationDate = lens _dspsrsCreationDate (\s a -> s {_dspsrsCreationDate = a}) . mapping _Time

-- | /Please use 'DescribeSecurityProfileResponse$additionalMetricsToRetainV2' instead./  A list of metrics whose data is retained (stored). By default, data is retained for any metric used in the profile's @behaviors@ , but it is also retained for any metric specified here.
dspsrsAdditionalMetricsToRetain :: Lens' DescribeSecurityProfileResponse [Text]
dspsrsAdditionalMetricsToRetain = lens _dspsrsAdditionalMetricsToRetain (\s a -> s {_dspsrsAdditionalMetricsToRetain = a}) . _Default . _Coerce

-- | The ARN of the security profile.
dspsrsSecurityProfileARN :: Lens' DescribeSecurityProfileResponse (Maybe Text)
dspsrsSecurityProfileARN = lens _dspsrsSecurityProfileARN (\s a -> s {_dspsrsSecurityProfileARN = a})

-- | A description of the security profile (associated with the security profile when it was created or updated).
dspsrsSecurityProfileDescription :: Lens' DescribeSecurityProfileResponse (Maybe Text)
dspsrsSecurityProfileDescription = lens _dspsrsSecurityProfileDescription (\s a -> s {_dspsrsSecurityProfileDescription = a})

-- | -- | The response status code.
dspsrsResponseStatus :: Lens' DescribeSecurityProfileResponse Int
dspsrsResponseStatus = lens _dspsrsResponseStatus (\s a -> s {_dspsrsResponseStatus = a})

instance NFData DescribeSecurityProfileResponse
