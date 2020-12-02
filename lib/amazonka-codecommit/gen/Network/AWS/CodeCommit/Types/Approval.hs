{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Approval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Approval where

import Network.AWS.CodeCommit.Types.ApprovalState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a specific approval on a pull request.
--
--
--
-- /See:/ 'approval' smart constructor.
data Approval = Approval'
  { _aApprovalState ::
      !(Maybe ApprovalState),
    _aUserARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Approval' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aApprovalState' - The state of the approval, APPROVE or REVOKE. REVOKE states are not stored.
--
-- * 'aUserARN' - The Amazon Resource Name (ARN) of the user.
approval ::
  Approval
approval =
  Approval' {_aApprovalState = Nothing, _aUserARN = Nothing}

-- | The state of the approval, APPROVE or REVOKE. REVOKE states are not stored.
aApprovalState :: Lens' Approval (Maybe ApprovalState)
aApprovalState = lens _aApprovalState (\s a -> s {_aApprovalState = a})

-- | The Amazon Resource Name (ARN) of the user.
aUserARN :: Lens' Approval (Maybe Text)
aUserARN = lens _aUserARN (\s a -> s {_aUserARN = a})

instance FromJSON Approval where
  parseJSON =
    withObject
      "Approval"
      ( \x ->
          Approval' <$> (x .:? "approvalState") <*> (x .:? "userArn")
      )

instance Hashable Approval

instance NFData Approval
