{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ServiceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ServiceInfo where

import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.Evidence
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains additional information about the generated finding.
--
--
--
-- /See:/ 'serviceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { _siCount :: !(Maybe Int),
    _siEventFirstSeen :: !(Maybe Text),
    _siAction :: !(Maybe Action),
    _siDetectorId :: !(Maybe Text),
    _siServiceName :: !(Maybe Text),
    _siUserFeedback :: !(Maybe Text),
    _siEvidence :: !(Maybe Evidence),
    _siEventLastSeen :: !(Maybe Text),
    _siResourceRole :: !(Maybe Text),
    _siArchived :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siCount' - The total count of the occurrences of this finding type.
--
-- * 'siEventFirstSeen' - The first-seen timestamp of the activity that prompted GuardDuty to generate this finding.
--
-- * 'siAction' - Information about the activity that is described in a finding.
--
-- * 'siDetectorId' - The detector ID for the GuardDuty service.
--
-- * 'siServiceName' - The name of the AWS service (GuardDuty) that generated a finding.
--
-- * 'siUserFeedback' - Feedback that was submitted about the finding.
--
-- * 'siEvidence' - An evidence object associated with the service.
--
-- * 'siEventLastSeen' - The last-seen timestamp of the activity that prompted GuardDuty to generate this finding.
--
-- * 'siResourceRole' - The resource role information for this finding.
--
-- * 'siArchived' - Indicates whether this finding is archived.
serviceInfo ::
  ServiceInfo
serviceInfo =
  ServiceInfo'
    { _siCount = Nothing,
      _siEventFirstSeen = Nothing,
      _siAction = Nothing,
      _siDetectorId = Nothing,
      _siServiceName = Nothing,
      _siUserFeedback = Nothing,
      _siEvidence = Nothing,
      _siEventLastSeen = Nothing,
      _siResourceRole = Nothing,
      _siArchived = Nothing
    }

-- | The total count of the occurrences of this finding type.
siCount :: Lens' ServiceInfo (Maybe Int)
siCount = lens _siCount (\s a -> s {_siCount = a})

-- | The first-seen timestamp of the activity that prompted GuardDuty to generate this finding.
siEventFirstSeen :: Lens' ServiceInfo (Maybe Text)
siEventFirstSeen = lens _siEventFirstSeen (\s a -> s {_siEventFirstSeen = a})

-- | Information about the activity that is described in a finding.
siAction :: Lens' ServiceInfo (Maybe Action)
siAction = lens _siAction (\s a -> s {_siAction = a})

-- | The detector ID for the GuardDuty service.
siDetectorId :: Lens' ServiceInfo (Maybe Text)
siDetectorId = lens _siDetectorId (\s a -> s {_siDetectorId = a})

-- | The name of the AWS service (GuardDuty) that generated a finding.
siServiceName :: Lens' ServiceInfo (Maybe Text)
siServiceName = lens _siServiceName (\s a -> s {_siServiceName = a})

-- | Feedback that was submitted about the finding.
siUserFeedback :: Lens' ServiceInfo (Maybe Text)
siUserFeedback = lens _siUserFeedback (\s a -> s {_siUserFeedback = a})

-- | An evidence object associated with the service.
siEvidence :: Lens' ServiceInfo (Maybe Evidence)
siEvidence = lens _siEvidence (\s a -> s {_siEvidence = a})

-- | The last-seen timestamp of the activity that prompted GuardDuty to generate this finding.
siEventLastSeen :: Lens' ServiceInfo (Maybe Text)
siEventLastSeen = lens _siEventLastSeen (\s a -> s {_siEventLastSeen = a})

-- | The resource role information for this finding.
siResourceRole :: Lens' ServiceInfo (Maybe Text)
siResourceRole = lens _siResourceRole (\s a -> s {_siResourceRole = a})

-- | Indicates whether this finding is archived.
siArchived :: Lens' ServiceInfo (Maybe Bool)
siArchived = lens _siArchived (\s a -> s {_siArchived = a})

instance FromJSON ServiceInfo where
  parseJSON =
    withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            <$> (x .:? "count")
            <*> (x .:? "eventFirstSeen")
            <*> (x .:? "action")
            <*> (x .:? "detectorId")
            <*> (x .:? "serviceName")
            <*> (x .:? "userFeedback")
            <*> (x .:? "evidence")
            <*> (x .:? "eventLastSeen")
            <*> (x .:? "resourceRole")
            <*> (x .:? "archived")
      )

instance Hashable ServiceInfo

instance NFData ServiceInfo
