{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about an event for an approval rule.
--
--
--
-- /See:/ 'approvalRuleEventMetadata' smart constructor.
data ApprovalRuleEventMetadata = ApprovalRuleEventMetadata'
  { _aremApprovalRuleName ::
      !(Maybe Text),
    _aremApprovalRuleId :: !(Maybe Text),
    _aremApprovalRuleContent ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApprovalRuleEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aremApprovalRuleName' - The name of the approval rule.
--
-- * 'aremApprovalRuleId' - The system-generated ID of the approval rule.
--
-- * 'aremApprovalRuleContent' - The content of the approval rule.
approvalRuleEventMetadata ::
  ApprovalRuleEventMetadata
approvalRuleEventMetadata =
  ApprovalRuleEventMetadata'
    { _aremApprovalRuleName = Nothing,
      _aremApprovalRuleId = Nothing,
      _aremApprovalRuleContent = Nothing
    }

-- | The name of the approval rule.
aremApprovalRuleName :: Lens' ApprovalRuleEventMetadata (Maybe Text)
aremApprovalRuleName = lens _aremApprovalRuleName (\s a -> s {_aremApprovalRuleName = a})

-- | The system-generated ID of the approval rule.
aremApprovalRuleId :: Lens' ApprovalRuleEventMetadata (Maybe Text)
aremApprovalRuleId = lens _aremApprovalRuleId (\s a -> s {_aremApprovalRuleId = a})

-- | The content of the approval rule.
aremApprovalRuleContent :: Lens' ApprovalRuleEventMetadata (Maybe Text)
aremApprovalRuleContent = lens _aremApprovalRuleContent (\s a -> s {_aremApprovalRuleContent = a})

instance FromJSON ApprovalRuleEventMetadata where
  parseJSON =
    withObject
      "ApprovalRuleEventMetadata"
      ( \x ->
          ApprovalRuleEventMetadata'
            <$> (x .:? "approvalRuleName")
            <*> (x .:? "approvalRuleId")
            <*> (x .:? "approvalRuleContent")
      )

instance Hashable ApprovalRuleEventMetadata

instance NFData ApprovalRuleEventMetadata
