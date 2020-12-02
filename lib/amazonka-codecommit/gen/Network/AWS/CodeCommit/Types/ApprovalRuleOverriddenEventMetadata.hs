{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata where

import Network.AWS.CodeCommit.Types.OverrideStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about an override event for approval rules for a pull request.
--
--
--
-- /See:/ 'approvalRuleOverriddenEventMetadata' smart constructor.
data ApprovalRuleOverriddenEventMetadata = ApprovalRuleOverriddenEventMetadata'
  { _aroemOverrideStatus ::
      !( Maybe
           OverrideStatus
       ),
    _aroemRevisionId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApprovalRuleOverriddenEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aroemOverrideStatus' - The status of the override event.
--
-- * 'aroemRevisionId' - The revision ID of the pull request when the override event occurred.
approvalRuleOverriddenEventMetadata ::
  ApprovalRuleOverriddenEventMetadata
approvalRuleOverriddenEventMetadata =
  ApprovalRuleOverriddenEventMetadata'
    { _aroemOverrideStatus =
        Nothing,
      _aroemRevisionId = Nothing
    }

-- | The status of the override event.
aroemOverrideStatus :: Lens' ApprovalRuleOverriddenEventMetadata (Maybe OverrideStatus)
aroemOverrideStatus = lens _aroemOverrideStatus (\s a -> s {_aroemOverrideStatus = a})

-- | The revision ID of the pull request when the override event occurred.
aroemRevisionId :: Lens' ApprovalRuleOverriddenEventMetadata (Maybe Text)
aroemRevisionId = lens _aroemRevisionId (\s a -> s {_aroemRevisionId = a})

instance FromJSON ApprovalRuleOverriddenEventMetadata where
  parseJSON =
    withObject
      "ApprovalRuleOverriddenEventMetadata"
      ( \x ->
          ApprovalRuleOverriddenEventMetadata'
            <$> (x .:? "overrideStatus") <*> (x .:? "revisionId")
      )

instance Hashable ApprovalRuleOverriddenEventMetadata

instance NFData ApprovalRuleOverriddenEventMetadata
