{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Evaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Evaluation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the approval rules applied to a pull request and whether conditions have been met.
--
--
--
-- /See:/ 'evaluation' smart constructor.
data Evaluation = Evaluation'
  { _eApprovalRulesSatisfied ::
      !(Maybe [Text]),
    _eApprovalRulesNotSatisfied :: !(Maybe [Text]),
    _eApproved :: !(Maybe Bool),
    _eOverridden :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eApprovalRulesSatisfied' - The names of the approval rules that have had their conditions met.
--
-- * 'eApprovalRulesNotSatisfied' - The names of the approval rules that have not had their conditions met.
--
-- * 'eApproved' - Whether the state of the pull request is approved.
--
-- * 'eOverridden' - Whether the approval rule requirements for the pull request have been overridden and no longer need to be met.
evaluation ::
  Evaluation
evaluation =
  Evaluation'
    { _eApprovalRulesSatisfied = Nothing,
      _eApprovalRulesNotSatisfied = Nothing,
      _eApproved = Nothing,
      _eOverridden = Nothing
    }

-- | The names of the approval rules that have had their conditions met.
eApprovalRulesSatisfied :: Lens' Evaluation [Text]
eApprovalRulesSatisfied = lens _eApprovalRulesSatisfied (\s a -> s {_eApprovalRulesSatisfied = a}) . _Default . _Coerce

-- | The names of the approval rules that have not had their conditions met.
eApprovalRulesNotSatisfied :: Lens' Evaluation [Text]
eApprovalRulesNotSatisfied = lens _eApprovalRulesNotSatisfied (\s a -> s {_eApprovalRulesNotSatisfied = a}) . _Default . _Coerce

-- | Whether the state of the pull request is approved.
eApproved :: Lens' Evaluation (Maybe Bool)
eApproved = lens _eApproved (\s a -> s {_eApproved = a})

-- | Whether the approval rule requirements for the pull request have been overridden and no longer need to be met.
eOverridden :: Lens' Evaluation (Maybe Bool)
eOverridden = lens _eOverridden (\s a -> s {_eOverridden = a})

instance FromJSON Evaluation where
  parseJSON =
    withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            <$> (x .:? "approvalRulesSatisfied" .!= mempty)
            <*> (x .:? "approvalRulesNotSatisfied" .!= mempty)
            <*> (x .:? "approved")
            <*> (x .:? "overridden")
      )

instance Hashable Evaluation

instance NFData Evaluation
