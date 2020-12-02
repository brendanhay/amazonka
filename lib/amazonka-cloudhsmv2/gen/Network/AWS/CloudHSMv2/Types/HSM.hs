{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.HSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.HSM where

import Network.AWS.CloudHSMv2.Types.HSMState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.
--
--
--
-- /See:/ 'hsm' smart constructor.
data HSM = HSM'
  { _hsmStateMessage :: !(Maybe Text),
    _hsmState :: !(Maybe HSMState),
    _hsmEniId :: !(Maybe Text),
    _hsmSubnetId :: !(Maybe Text),
    _hsmAvailabilityZone :: !(Maybe Text),
    _hsmClusterId :: !(Maybe Text),
    _hsmEniIP :: !(Maybe Text),
    _hsmHSMId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsmStateMessage' - A description of the HSM's state.
--
-- * 'hsmState' - The HSM's state.
--
-- * 'hsmEniId' - The identifier (ID) of the HSM's elastic network interface (ENI).
--
-- * 'hsmSubnetId' - The subnet that contains the HSM's elastic network interface (ENI).
--
-- * 'hsmAvailabilityZone' - The Availability Zone that contains the HSM.
--
-- * 'hsmClusterId' - The identifier (ID) of the cluster that contains the HSM.
--
-- * 'hsmEniIP' - The IP address of the HSM's elastic network interface (ENI).
--
-- * 'hsmHSMId' - The HSM's identifier (ID).
hsm ::
  -- | 'hsmHSMId'
  Text ->
  HSM
hsm pHSMId_ =
  HSM'
    { _hsmStateMessage = Nothing,
      _hsmState = Nothing,
      _hsmEniId = Nothing,
      _hsmSubnetId = Nothing,
      _hsmAvailabilityZone = Nothing,
      _hsmClusterId = Nothing,
      _hsmEniIP = Nothing,
      _hsmHSMId = pHSMId_
    }

-- | A description of the HSM's state.
hsmStateMessage :: Lens' HSM (Maybe Text)
hsmStateMessage = lens _hsmStateMessage (\s a -> s {_hsmStateMessage = a})

-- | The HSM's state.
hsmState :: Lens' HSM (Maybe HSMState)
hsmState = lens _hsmState (\s a -> s {_hsmState = a})

-- | The identifier (ID) of the HSM's elastic network interface (ENI).
hsmEniId :: Lens' HSM (Maybe Text)
hsmEniId = lens _hsmEniId (\s a -> s {_hsmEniId = a})

-- | The subnet that contains the HSM's elastic network interface (ENI).
hsmSubnetId :: Lens' HSM (Maybe Text)
hsmSubnetId = lens _hsmSubnetId (\s a -> s {_hsmSubnetId = a})

-- | The Availability Zone that contains the HSM.
hsmAvailabilityZone :: Lens' HSM (Maybe Text)
hsmAvailabilityZone = lens _hsmAvailabilityZone (\s a -> s {_hsmAvailabilityZone = a})

-- | The identifier (ID) of the cluster that contains the HSM.
hsmClusterId :: Lens' HSM (Maybe Text)
hsmClusterId = lens _hsmClusterId (\s a -> s {_hsmClusterId = a})

-- | The IP address of the HSM's elastic network interface (ENI).
hsmEniIP :: Lens' HSM (Maybe Text)
hsmEniIP = lens _hsmEniIP (\s a -> s {_hsmEniIP = a})

-- | The HSM's identifier (ID).
hsmHSMId :: Lens' HSM Text
hsmHSMId = lens _hsmHSMId (\s a -> s {_hsmHSMId = a})

instance FromJSON HSM where
  parseJSON =
    withObject
      "HSM"
      ( \x ->
          HSM'
            <$> (x .:? "StateMessage")
            <*> (x .:? "State")
            <*> (x .:? "EniId")
            <*> (x .:? "SubnetId")
            <*> (x .:? "AvailabilityZone")
            <*> (x .:? "ClusterId")
            <*> (x .:? "EniIp")
            <*> (x .: "HsmId")
      )

instance Hashable HSM

instance NFData HSM
