{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Participants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Participants where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.GroupMetadata
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the users or user groups.
--
--
--
-- /See:/ 'participants' smart constructor.
data Participants = Participants'
  { _pGroups ::
      !(Maybe [GroupMetadata]),
    _pUsers :: !(Maybe [UserMetadata])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Participants' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pGroups' - The list of user groups.
--
-- * 'pUsers' - The list of users.
participants ::
  Participants
participants = Participants' {_pGroups = Nothing, _pUsers = Nothing}

-- | The list of user groups.
pGroups :: Lens' Participants [GroupMetadata]
pGroups = lens _pGroups (\s a -> s {_pGroups = a}) . _Default . _Coerce

-- | The list of users.
pUsers :: Lens' Participants [UserMetadata]
pUsers = lens _pUsers (\s a -> s {_pUsers = a}) . _Default . _Coerce

instance FromJSON Participants where
  parseJSON =
    withObject
      "Participants"
      ( \x ->
          Participants'
            <$> (x .:? "Groups" .!= mempty) <*> (x .:? "Users" .!= mempty)
      )

instance Hashable Participants

instance NFData Participants
