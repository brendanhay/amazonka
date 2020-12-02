{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionHistoryDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionHistoryDetails where

import Network.AWS.Budgets.Types.Action
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of details of the event.
--
--
--
-- /See:/ 'actionHistoryDetails' smart constructor.
data ActionHistoryDetails = ActionHistoryDetails'
  { _ahdMessage ::
      !Text,
    _ahdAction :: !Action
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionHistoryDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahdMessage' - Undocumented member.
--
-- * 'ahdAction' - The budget action resource.
actionHistoryDetails ::
  -- | 'ahdMessage'
  Text ->
  -- | 'ahdAction'
  Action ->
  ActionHistoryDetails
actionHistoryDetails pMessage_ pAction_ =
  ActionHistoryDetails'
    { _ahdMessage = pMessage_,
      _ahdAction = pAction_
    }

-- | Undocumented member.
ahdMessage :: Lens' ActionHistoryDetails Text
ahdMessage = lens _ahdMessage (\s a -> s {_ahdMessage = a})

-- | The budget action resource.
ahdAction :: Lens' ActionHistoryDetails Action
ahdAction = lens _ahdAction (\s a -> s {_ahdAction = a})

instance FromJSON ActionHistoryDetails where
  parseJSON =
    withObject
      "ActionHistoryDetails"
      ( \x ->
          ActionHistoryDetails' <$> (x .: "Message") <*> (x .: "Action")
      )

instance Hashable ActionHistoryDetails

instance NFData ActionHistoryDetails
