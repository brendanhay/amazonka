{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceData where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchComplianceDataState

-- | Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.
--
--
--
-- /See:/ 'patchComplianceData' smart constructor.
data PatchComplianceData = PatchComplianceData'
  { _pcdCVEIds ::
      !(Maybe Text),
    _pcdTitle :: !Text,
    _pcdKBId :: !Text,
    _pcdClassification :: !Text,
    _pcdSeverity :: !Text,
    _pcdState :: !PatchComplianceDataState,
    _pcdInstalledTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchComplianceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcdCVEIds' - The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
--
-- * 'pcdTitle' - The title of the patch.
--
-- * 'pcdKBId' - The operating system-specific ID of the patch.
--
-- * 'pcdClassification' - The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
--
-- * 'pcdSeverity' - The severity of the patch (for example, Critical, Important, Moderate).
--
-- * 'pcdState' - The state of the patch on the instance, such as INSTALLED or FAILED. For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
--
-- * 'pcdInstalledTime' - The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
patchComplianceData ::
  -- | 'pcdTitle'
  Text ->
  -- | 'pcdKBId'
  Text ->
  -- | 'pcdClassification'
  Text ->
  -- | 'pcdSeverity'
  Text ->
  -- | 'pcdState'
  PatchComplianceDataState ->
  -- | 'pcdInstalledTime'
  UTCTime ->
  PatchComplianceData
patchComplianceData
  pTitle_
  pKBId_
  pClassification_
  pSeverity_
  pState_
  pInstalledTime_ =
    PatchComplianceData'
      { _pcdCVEIds = Nothing,
        _pcdTitle = pTitle_,
        _pcdKBId = pKBId_,
        _pcdClassification = pClassification_,
        _pcdSeverity = pSeverity_,
        _pcdState = pState_,
        _pcdInstalledTime = _Time # pInstalledTime_
      }

-- | The IDs of one or more Common Vulnerabilities and Exposure (CVE) issues that are resolved by the patch.
pcdCVEIds :: Lens' PatchComplianceData (Maybe Text)
pcdCVEIds = lens _pcdCVEIds (\s a -> s {_pcdCVEIds = a})

-- | The title of the patch.
pcdTitle :: Lens' PatchComplianceData Text
pcdTitle = lens _pcdTitle (\s a -> s {_pcdTitle = a})

-- | The operating system-specific ID of the patch.
pcdKBId :: Lens' PatchComplianceData Text
pcdKBId = lens _pcdKBId (\s a -> s {_pcdKBId = a})

-- | The classification of the patch (for example, SecurityUpdates, Updates, CriticalUpdates).
pcdClassification :: Lens' PatchComplianceData Text
pcdClassification = lens _pcdClassification (\s a -> s {_pcdClassification = a})

-- | The severity of the patch (for example, Critical, Important, Moderate).
pcdSeverity :: Lens' PatchComplianceData Text
pcdSeverity = lens _pcdSeverity (\s a -> s {_pcdSeverity = a})

-- | The state of the patch on the instance, such as INSTALLED or FAILED. For descriptions of each patch state, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-compliance-about.html#sysman-compliance-monitor-patch About patch compliance> in the /AWS Systems Manager User Guide/ .
pcdState :: Lens' PatchComplianceData PatchComplianceDataState
pcdState = lens _pcdState (\s a -> s {_pcdState = a})

-- | The date/time the patch was installed on the instance. Note that not all operating systems provide this level of information.
pcdInstalledTime :: Lens' PatchComplianceData UTCTime
pcdInstalledTime = lens _pcdInstalledTime (\s a -> s {_pcdInstalledTime = a}) . _Time

instance FromJSON PatchComplianceData where
  parseJSON =
    withObject
      "PatchComplianceData"
      ( \x ->
          PatchComplianceData'
            <$> (x .:? "CVEIds")
            <*> (x .: "Title")
            <*> (x .: "KBId")
            <*> (x .: "Classification")
            <*> (x .: "Severity")
            <*> (x .: "State")
            <*> (x .: "InstalledTime")
      )

instance Hashable PatchComplianceData

instance NFData PatchComplianceData
