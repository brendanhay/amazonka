{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ShareResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ShareResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.RoleType
import Network.AWS.WorkDocs.Types.ShareStatusType

-- | Describes the share results of a resource.
--
--
--
-- /See:/ 'shareResult' smart constructor.
data ShareResult = ShareResult'
  { _srStatus ::
      !(Maybe ShareStatusType),
    _srPrincipalId :: !(Maybe Text),
    _srInviteePrincipalId :: !(Maybe Text),
    _srRole :: !(Maybe RoleType),
    _srStatusMessage :: !(Maybe (Sensitive Text)),
    _srShareId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShareResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus' - The status.
--
-- * 'srPrincipalId' - The ID of the principal.
--
-- * 'srInviteePrincipalId' - The ID of the invited user.
--
-- * 'srRole' - The role.
--
-- * 'srStatusMessage' - The status message.
--
-- * 'srShareId' - The ID of the resource that was shared.
shareResult ::
  ShareResult
shareResult =
  ShareResult'
    { _srStatus = Nothing,
      _srPrincipalId = Nothing,
      _srInviteePrincipalId = Nothing,
      _srRole = Nothing,
      _srStatusMessage = Nothing,
      _srShareId = Nothing
    }

-- | The status.
srStatus :: Lens' ShareResult (Maybe ShareStatusType)
srStatus = lens _srStatus (\s a -> s {_srStatus = a})

-- | The ID of the principal.
srPrincipalId :: Lens' ShareResult (Maybe Text)
srPrincipalId = lens _srPrincipalId (\s a -> s {_srPrincipalId = a})

-- | The ID of the invited user.
srInviteePrincipalId :: Lens' ShareResult (Maybe Text)
srInviteePrincipalId = lens _srInviteePrincipalId (\s a -> s {_srInviteePrincipalId = a})

-- | The role.
srRole :: Lens' ShareResult (Maybe RoleType)
srRole = lens _srRole (\s a -> s {_srRole = a})

-- | The status message.
srStatusMessage :: Lens' ShareResult (Maybe Text)
srStatusMessage = lens _srStatusMessage (\s a -> s {_srStatusMessage = a}) . mapping _Sensitive

-- | The ID of the resource that was shared.
srShareId :: Lens' ShareResult (Maybe Text)
srShareId = lens _srShareId (\s a -> s {_srShareId = a})

instance FromJSON ShareResult where
  parseJSON =
    withObject
      "ShareResult"
      ( \x ->
          ShareResult'
            <$> (x .:? "Status")
            <*> (x .:? "PrincipalId")
            <*> (x .:? "InviteePrincipalId")
            <*> (x .:? "Role")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "ShareId")
      )

instance Hashable ShareResult

instance NFData ShareResult
