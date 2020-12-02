{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationActionParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationActionParams where

import Network.AWS.IoT.Types.AddThingsToThingGroupParams
import Network.AWS.IoT.Types.EnableIOTLoggingParams
import Network.AWS.IoT.Types.PublishFindingToSNSParams
import Network.AWS.IoT.Types.ReplaceDefaultPolicyVersionParams
import Network.AWS.IoT.Types.UpdateCACertificateParams
import Network.AWS.IoT.Types.UpdateDeviceCertificateParams
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The set of parameters for this mitigation action. You can specify only one type of parameter (in other words, you can apply only one action for each defined mitigation action).
--
--
--
-- /See:/ 'mitigationActionParams' smart constructor.
data MitigationActionParams = MitigationActionParams'
  { _mapEnableIOTLoggingParams ::
      !(Maybe EnableIOTLoggingParams),
    _mapAddThingsToThingGroupParams ::
      !(Maybe AddThingsToThingGroupParams),
    _mapUpdateCACertificateParams ::
      !(Maybe UpdateCACertificateParams),
    _mapUpdateDeviceCertificateParams ::
      !(Maybe UpdateDeviceCertificateParams),
    _mapReplaceDefaultPolicyVersionParams ::
      !(Maybe ReplaceDefaultPolicyVersionParams),
    _mapPublishFindingToSNSParams ::
      !(Maybe PublishFindingToSNSParams)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MitigationActionParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mapEnableIOTLoggingParams' - Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
--
-- * 'mapAddThingsToThingGroupParams' - Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
--
-- * 'mapUpdateCACertificateParams' - Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
--
-- * 'mapUpdateDeviceCertificateParams' - Parameters to define a mitigation action that changes the state of the device certificate to inactive.
--
-- * 'mapReplaceDefaultPolicyVersionParams' - Parameters to define a mitigation action that adds a blank policy to restrict permissions.
--
-- * 'mapPublishFindingToSNSParams' - Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
mitigationActionParams ::
  MitigationActionParams
mitigationActionParams =
  MitigationActionParams'
    { _mapEnableIOTLoggingParams = Nothing,
      _mapAddThingsToThingGroupParams = Nothing,
      _mapUpdateCACertificateParams = Nothing,
      _mapUpdateDeviceCertificateParams = Nothing,
      _mapReplaceDefaultPolicyVersionParams = Nothing,
      _mapPublishFindingToSNSParams = Nothing
    }

-- | Parameters to define a mitigation action that enables AWS IoT logging at a specified level of detail.
mapEnableIOTLoggingParams :: Lens' MitigationActionParams (Maybe EnableIOTLoggingParams)
mapEnableIOTLoggingParams = lens _mapEnableIOTLoggingParams (\s a -> s {_mapEnableIOTLoggingParams = a})

-- | Parameters to define a mitigation action that moves devices associated with a certificate to one or more specified thing groups, typically for quarantine.
mapAddThingsToThingGroupParams :: Lens' MitigationActionParams (Maybe AddThingsToThingGroupParams)
mapAddThingsToThingGroupParams = lens _mapAddThingsToThingGroupParams (\s a -> s {_mapAddThingsToThingGroupParams = a})

-- | Parameters to define a mitigation action that changes the state of the CA certificate to inactive.
mapUpdateCACertificateParams :: Lens' MitigationActionParams (Maybe UpdateCACertificateParams)
mapUpdateCACertificateParams = lens _mapUpdateCACertificateParams (\s a -> s {_mapUpdateCACertificateParams = a})

-- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
mapUpdateDeviceCertificateParams :: Lens' MitigationActionParams (Maybe UpdateDeviceCertificateParams)
mapUpdateDeviceCertificateParams = lens _mapUpdateDeviceCertificateParams (\s a -> s {_mapUpdateDeviceCertificateParams = a})

-- | Parameters to define a mitigation action that adds a blank policy to restrict permissions.
mapReplaceDefaultPolicyVersionParams :: Lens' MitigationActionParams (Maybe ReplaceDefaultPolicyVersionParams)
mapReplaceDefaultPolicyVersionParams = lens _mapReplaceDefaultPolicyVersionParams (\s a -> s {_mapReplaceDefaultPolicyVersionParams = a})

-- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
mapPublishFindingToSNSParams :: Lens' MitigationActionParams (Maybe PublishFindingToSNSParams)
mapPublishFindingToSNSParams = lens _mapPublishFindingToSNSParams (\s a -> s {_mapPublishFindingToSNSParams = a})

instance FromJSON MitigationActionParams where
  parseJSON =
    withObject
      "MitigationActionParams"
      ( \x ->
          MitigationActionParams'
            <$> (x .:? "enableIoTLoggingParams")
            <*> (x .:? "addThingsToThingGroupParams")
            <*> (x .:? "updateCACertificateParams")
            <*> (x .:? "updateDeviceCertificateParams")
            <*> (x .:? "replaceDefaultPolicyVersionParams")
            <*> (x .:? "publishFindingToSnsParams")
      )

instance Hashable MitigationActionParams

instance NFData MitigationActionParams

instance ToJSON MitigationActionParams where
  toJSON MitigationActionParams' {..} =
    object
      ( catMaybes
          [ ("enableIoTLoggingParams" .=) <$> _mapEnableIOTLoggingParams,
            ("addThingsToThingGroupParams" .=)
              <$> _mapAddThingsToThingGroupParams,
            ("updateCACertificateParams" .=) <$> _mapUpdateCACertificateParams,
            ("updateDeviceCertificateParams" .=)
              <$> _mapUpdateDeviceCertificateParams,
            ("replaceDefaultPolicyVersionParams" .=)
              <$> _mapReplaceDefaultPolicyVersionParams,
            ("publishFindingToSnsParams" .=)
              <$> _mapPublishFindingToSNSParams
          ]
      )
