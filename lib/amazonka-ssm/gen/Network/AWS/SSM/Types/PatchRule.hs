{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchFilterGroup

-- | Defines an approval rule for a patch baseline.
--
--
--
-- /See:/ 'patchRule' smart constructor.
data PatchRule = PatchRule'
  { _prApproveAfterDays :: !(Maybe Nat),
    _prApproveUntilDate :: !(Maybe Text),
    _prEnableNonSecurity :: !(Maybe Bool),
    _prComplianceLevel :: !(Maybe PatchComplianceLevel),
    _prPatchFilterGroup :: !PatchFilterGroup
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prApproveAfterDays' - The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
--
-- * 'prApproveUntilDate' - The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server. Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
--
-- * 'prEnableNonSecurity' - For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
--
-- * 'prComplianceLevel' - A compliance severity level for all approved patches in a patch baseline.
--
-- * 'prPatchFilterGroup' - The patch filter group that defines the criteria for the rule.
patchRule ::
  -- | 'prPatchFilterGroup'
  PatchFilterGroup ->
  PatchRule
patchRule pPatchFilterGroup_ =
  PatchRule'
    { _prApproveAfterDays = Nothing,
      _prApproveUntilDate = Nothing,
      _prEnableNonSecurity = Nothing,
      _prComplianceLevel = Nothing,
      _prPatchFilterGroup = pPatchFilterGroup_
    }

-- | The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
prApproveAfterDays :: Lens' PatchRule (Maybe Natural)
prApproveAfterDays = lens _prApproveAfterDays (\s a -> s {_prApproveAfterDays = a}) . mapping _Nat

-- | The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server. Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
prApproveUntilDate :: Lens' PatchRule (Maybe Text)
prApproveUntilDate = lens _prApproveUntilDate (\s a -> s {_prApproveUntilDate = a})

-- | For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
prEnableNonSecurity :: Lens' PatchRule (Maybe Bool)
prEnableNonSecurity = lens _prEnableNonSecurity (\s a -> s {_prEnableNonSecurity = a})

-- | A compliance severity level for all approved patches in a patch baseline.
prComplianceLevel :: Lens' PatchRule (Maybe PatchComplianceLevel)
prComplianceLevel = lens _prComplianceLevel (\s a -> s {_prComplianceLevel = a})

-- | The patch filter group that defines the criteria for the rule.
prPatchFilterGroup :: Lens' PatchRule PatchFilterGroup
prPatchFilterGroup = lens _prPatchFilterGroup (\s a -> s {_prPatchFilterGroup = a})

instance FromJSON PatchRule where
  parseJSON =
    withObject
      "PatchRule"
      ( \x ->
          PatchRule'
            <$> (x .:? "ApproveAfterDays")
            <*> (x .:? "ApproveUntilDate")
            <*> (x .:? "EnableNonSecurity")
            <*> (x .:? "ComplianceLevel")
            <*> (x .: "PatchFilterGroup")
      )

instance Hashable PatchRule

instance NFData PatchRule

instance ToJSON PatchRule where
  toJSON PatchRule' {..} =
    object
      ( catMaybes
          [ ("ApproveAfterDays" .=) <$> _prApproveAfterDays,
            ("ApproveUntilDate" .=) <$> _prApproveUntilDate,
            ("EnableNonSecurity" .=) <$> _prEnableNonSecurity,
            ("ComplianceLevel" .=) <$> _prComplianceLevel,
            Just ("PatchFilterGroup" .= _prPatchFilterGroup)
          ]
      )
