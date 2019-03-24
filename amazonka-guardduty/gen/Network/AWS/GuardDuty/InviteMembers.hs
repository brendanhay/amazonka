{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.InviteMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.
module Network.AWS.GuardDuty.InviteMembers
    (
    -- * Creating a Request
      inviteMembers
    , InviteMembers
    -- * Request Lenses
    , imDisableEmailNotification
    , imMessage
    , imDetectorId
    , imAccountIds

    -- * Destructuring the Response
    , inviteMembersResponse
    , InviteMembersResponse
    -- * Response Lenses
    , imrsUnprocessedAccounts
    , imrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | InviteMembers request body.
--
-- /See:/ 'inviteMembers' smart constructor.
data InviteMembers = InviteMembers'
  { _imDisableEmailNotification :: !(Maybe Bool)
  , _imMessage                  :: !(Maybe Text)
  , _imDetectorId               :: !Text
  , _imAccountIds               :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InviteMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imDisableEmailNotification' - A boolean value that specifies whether you want to disable email notification to the accounts that you
