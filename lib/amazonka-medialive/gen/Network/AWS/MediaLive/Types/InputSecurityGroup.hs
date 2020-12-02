{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroup where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputSecurityGroupState
import Network.AWS.MediaLive.Types.InputWhitelistRule
import Network.AWS.Prelude

-- | An Input Security Group
--
-- /See:/ 'inputSecurityGroup' smart constructor.
data InputSecurityGroup = InputSecurityGroup'
  { _isgState ::
      !(Maybe InputSecurityGroupState),
    _isgARN :: !(Maybe Text),
    _isgInputs :: !(Maybe [Text]),
    _isgId :: !(Maybe Text),
    _isgWhitelistRules :: !(Maybe [InputWhitelistRule]),
    _isgTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isgState' - The current state of the Input Security Group.
--
-- * 'isgARN' - Unique ARN of Input Security Group
--
-- * 'isgInputs' - The list of inputs currently using this Input Security Group.
--
-- * 'isgId' - The Id of the Input Security Group
--
-- * 'isgWhitelistRules' - Whitelist rules and their sync status
--
-- * 'isgTags' - A collection of key-value pairs.
inputSecurityGroup ::
  InputSecurityGroup
inputSecurityGroup =
  InputSecurityGroup'
    { _isgState = Nothing,
      _isgARN = Nothing,
      _isgInputs = Nothing,
      _isgId = Nothing,
      _isgWhitelistRules = Nothing,
      _isgTags = Nothing
    }

-- | The current state of the Input Security Group.
isgState :: Lens' InputSecurityGroup (Maybe InputSecurityGroupState)
isgState = lens _isgState (\s a -> s {_isgState = a})

-- | Unique ARN of Input Security Group
isgARN :: Lens' InputSecurityGroup (Maybe Text)
isgARN = lens _isgARN (\s a -> s {_isgARN = a})

-- | The list of inputs currently using this Input Security Group.
isgInputs :: Lens' InputSecurityGroup [Text]
isgInputs = lens _isgInputs (\s a -> s {_isgInputs = a}) . _Default . _Coerce

-- | The Id of the Input Security Group
isgId :: Lens' InputSecurityGroup (Maybe Text)
isgId = lens _isgId (\s a -> s {_isgId = a})

-- | Whitelist rules and their sync status
isgWhitelistRules :: Lens' InputSecurityGroup [InputWhitelistRule]
isgWhitelistRules = lens _isgWhitelistRules (\s a -> s {_isgWhitelistRules = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
isgTags :: Lens' InputSecurityGroup (HashMap Text (Text))
isgTags = lens _isgTags (\s a -> s {_isgTags = a}) . _Default . _Map

instance FromJSON InputSecurityGroup where
  parseJSON =
    withObject
      "InputSecurityGroup"
      ( \x ->
          InputSecurityGroup'
            <$> (x .:? "state")
            <*> (x .:? "arn")
            <*> (x .:? "inputs" .!= mempty)
            <*> (x .:? "id")
            <*> (x .:? "whitelistRules" .!= mempty)
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable InputSecurityGroup

instance NFData InputSecurityGroup
