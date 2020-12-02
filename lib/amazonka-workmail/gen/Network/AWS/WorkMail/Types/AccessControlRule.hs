{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.AccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.AccessControlRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.AccessControlRuleEffect

-- | A rule that controls access to an Amazon WorkMail organization.
--
--
--
-- /See:/ 'accessControlRule' smart constructor.
data AccessControlRule = AccessControlRule'
  { _acrEffect ::
      !(Maybe AccessControlRuleEffect),
    _acrUserIds :: !(Maybe [Text]),
    _acrActions :: !(Maybe [Text]),
    _acrDateCreated :: !(Maybe POSIX),
    _acrName :: !(Maybe Text),
    _acrNotUserIds :: !(Maybe [Text]),
    _acrDateModified :: !(Maybe POSIX),
    _acrIPRanges :: !(Maybe [Text]),
    _acrNotIPRanges :: !(Maybe [Text]),
    _acrNotActions :: !(Maybe [Text]),
    _acrDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessControlRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acrEffect' - The rule effect.
--
-- * 'acrUserIds' - User IDs to include in the rule.
--
-- * 'acrActions' - Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- * 'acrDateCreated' - The date that the rule was created.
--
-- * 'acrName' - The rule name.
--
-- * 'acrNotUserIds' - User IDs to exclude from the rule.
--
-- * 'acrDateModified' - The date that the rule was modified.
--
-- * 'acrIPRanges' - IPv4 CIDR ranges to include in the rule.
--
-- * 'acrNotIPRanges' - IPv4 CIDR ranges to exclude from the rule.
--
-- * 'acrNotActions' - Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- * 'acrDescription' - The rule description.
accessControlRule ::
  AccessControlRule
accessControlRule =
  AccessControlRule'
    { _acrEffect = Nothing,
      _acrUserIds = Nothing,
      _acrActions = Nothing,
      _acrDateCreated = Nothing,
      _acrName = Nothing,
      _acrNotUserIds = Nothing,
      _acrDateModified = Nothing,
      _acrIPRanges = Nothing,
      _acrNotIPRanges = Nothing,
      _acrNotActions = Nothing,
      _acrDescription = Nothing
    }

-- | The rule effect.
acrEffect :: Lens' AccessControlRule (Maybe AccessControlRuleEffect)
acrEffect = lens _acrEffect (\s a -> s {_acrEffect = a})

-- | User IDs to include in the rule.
acrUserIds :: Lens' AccessControlRule [Text]
acrUserIds = lens _acrUserIds (\s a -> s {_acrUserIds = a}) . _Default . _Coerce

-- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
acrActions :: Lens' AccessControlRule [Text]
acrActions = lens _acrActions (\s a -> s {_acrActions = a}) . _Default . _Coerce

-- | The date that the rule was created.
acrDateCreated :: Lens' AccessControlRule (Maybe UTCTime)
acrDateCreated = lens _acrDateCreated (\s a -> s {_acrDateCreated = a}) . mapping _Time

-- | The rule name.
acrName :: Lens' AccessControlRule (Maybe Text)
acrName = lens _acrName (\s a -> s {_acrName = a})

-- | User IDs to exclude from the rule.
acrNotUserIds :: Lens' AccessControlRule [Text]
acrNotUserIds = lens _acrNotUserIds (\s a -> s {_acrNotUserIds = a}) . _Default . _Coerce

-- | The date that the rule was modified.
acrDateModified :: Lens' AccessControlRule (Maybe UTCTime)
acrDateModified = lens _acrDateModified (\s a -> s {_acrDateModified = a}) . mapping _Time

-- | IPv4 CIDR ranges to include in the rule.
acrIPRanges :: Lens' AccessControlRule [Text]
acrIPRanges = lens _acrIPRanges (\s a -> s {_acrIPRanges = a}) . _Default . _Coerce

-- | IPv4 CIDR ranges to exclude from the rule.
acrNotIPRanges :: Lens' AccessControlRule [Text]
acrNotIPRanges = lens _acrNotIPRanges (\s a -> s {_acrNotIPRanges = a}) . _Default . _Coerce

-- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
acrNotActions :: Lens' AccessControlRule [Text]
acrNotActions = lens _acrNotActions (\s a -> s {_acrNotActions = a}) . _Default . _Coerce

-- | The rule description.
acrDescription :: Lens' AccessControlRule (Maybe Text)
acrDescription = lens _acrDescription (\s a -> s {_acrDescription = a})

instance FromJSON AccessControlRule where
  parseJSON =
    withObject
      "AccessControlRule"
      ( \x ->
          AccessControlRule'
            <$> (x .:? "Effect")
            <*> (x .:? "UserIds" .!= mempty)
            <*> (x .:? "Actions" .!= mempty)
            <*> (x .:? "DateCreated")
            <*> (x .:? "Name")
            <*> (x .:? "NotUserIds" .!= mempty)
            <*> (x .:? "DateModified")
            <*> (x .:? "IpRanges" .!= mempty)
            <*> (x .:? "NotIpRanges" .!= mempty)
            <*> (x .:? "NotActions" .!= mempty)
            <*> (x .:? "Description")
      )

instance Hashable AccessControlRule

instance NFData AccessControlRule
