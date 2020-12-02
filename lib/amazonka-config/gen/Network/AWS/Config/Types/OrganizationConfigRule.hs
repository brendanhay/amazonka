{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRule where

import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An organization config rule that has information about config rules that AWS Config creates in member accounts.
--
--
--
-- /See:/ 'organizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { _ocrOrganizationManagedRuleMetadata ::
      !(Maybe OrganizationManagedRuleMetadata),
    _ocrExcludedAccounts :: !(Maybe [Text]),
    _ocrOrganizationCustomRuleMetadata ::
      !(Maybe OrganizationCustomRuleMetadata),
    _ocrLastUpdateTime :: !(Maybe POSIX),
    _ocrOrganizationConfigRuleName :: !Text,
    _ocrOrganizationConfigRuleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocrOrganizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
--
-- * 'ocrExcludedAccounts' - A comma-separated list of accounts excluded from organization config rule.
--
-- * 'ocrOrganizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
--
-- * 'ocrLastUpdateTime' - The timestamp of the last update.
--
-- * 'ocrOrganizationConfigRuleName' - The name that you assign to organization config rule.
--
-- * 'ocrOrganizationConfigRuleARN' - Amazon Resource Name (ARN) of organization config rule.
organizationConfigRule ::
  -- | 'ocrOrganizationConfigRuleName'
  Text ->
  -- | 'ocrOrganizationConfigRuleARN'
  Text ->
  OrganizationConfigRule
organizationConfigRule
  pOrganizationConfigRuleName_
  pOrganizationConfigRuleARN_ =
    OrganizationConfigRule'
      { _ocrOrganizationManagedRuleMetadata =
          Nothing,
        _ocrExcludedAccounts = Nothing,
        _ocrOrganizationCustomRuleMetadata = Nothing,
        _ocrLastUpdateTime = Nothing,
        _ocrOrganizationConfigRuleName = pOrganizationConfigRuleName_,
        _ocrOrganizationConfigRuleARN = pOrganizationConfigRuleARN_
      }

-- | An @OrganizationManagedRuleMetadata@ object.
ocrOrganizationManagedRuleMetadata :: Lens' OrganizationConfigRule (Maybe OrganizationManagedRuleMetadata)
ocrOrganizationManagedRuleMetadata = lens _ocrOrganizationManagedRuleMetadata (\s a -> s {_ocrOrganizationManagedRuleMetadata = a})

-- | A comma-separated list of accounts excluded from organization config rule.
ocrExcludedAccounts :: Lens' OrganizationConfigRule [Text]
ocrExcludedAccounts = lens _ocrExcludedAccounts (\s a -> s {_ocrExcludedAccounts = a}) . _Default . _Coerce

-- | An @OrganizationCustomRuleMetadata@ object.
ocrOrganizationCustomRuleMetadata :: Lens' OrganizationConfigRule (Maybe OrganizationCustomRuleMetadata)
ocrOrganizationCustomRuleMetadata = lens _ocrOrganizationCustomRuleMetadata (\s a -> s {_ocrOrganizationCustomRuleMetadata = a})

-- | The timestamp of the last update.
ocrLastUpdateTime :: Lens' OrganizationConfigRule (Maybe UTCTime)
ocrLastUpdateTime = lens _ocrLastUpdateTime (\s a -> s {_ocrLastUpdateTime = a}) . mapping _Time

-- | The name that you assign to organization config rule.
ocrOrganizationConfigRuleName :: Lens' OrganizationConfigRule Text
ocrOrganizationConfigRuleName = lens _ocrOrganizationConfigRuleName (\s a -> s {_ocrOrganizationConfigRuleName = a})

-- | Amazon Resource Name (ARN) of organization config rule.
ocrOrganizationConfigRuleARN :: Lens' OrganizationConfigRule Text
ocrOrganizationConfigRuleARN = lens _ocrOrganizationConfigRuleARN (\s a -> s {_ocrOrganizationConfigRuleARN = a})

instance FromJSON OrganizationConfigRule where
  parseJSON =
    withObject
      "OrganizationConfigRule"
      ( \x ->
          OrganizationConfigRule'
            <$> (x .:? "OrganizationManagedRuleMetadata")
            <*> (x .:? "ExcludedAccounts" .!= mempty)
            <*> (x .:? "OrganizationCustomRuleMetadata")
            <*> (x .:? "LastUpdateTime")
            <*> (x .: "OrganizationConfigRuleName")
            <*> (x .: "OrganizationConfigRuleArn")
      )

instance Hashable OrganizationConfigRule

instance NFData OrganizationConfigRule
