{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.FolderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.FolderConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.FolderName
import Network.AWS.WorkMail.Types.RetentionAction

-- | The configuration applied to an organization's folders by its retention policy.
--
--
--
-- /See:/ 'folderConfiguration' smart constructor.
data FolderConfiguration = FolderConfiguration'
  { _fcPeriod ::
      !(Maybe Nat),
    _fcName :: !FolderName,
    _fcAction :: !RetentionAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FolderConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcPeriod' - The period of time at which the folder configuration action is applied.
--
-- * 'fcName' - The folder name.
--
-- * 'fcAction' - The action to take on the folder contents at the end of the folder configuration period.
folderConfiguration ::
  -- | 'fcName'
  FolderName ->
  -- | 'fcAction'
  RetentionAction ->
  FolderConfiguration
folderConfiguration pName_ pAction_ =
  FolderConfiguration'
    { _fcPeriod = Nothing,
      _fcName = pName_,
      _fcAction = pAction_
    }

-- | The period of time at which the folder configuration action is applied.
fcPeriod :: Lens' FolderConfiguration (Maybe Natural)
fcPeriod = lens _fcPeriod (\s a -> s {_fcPeriod = a}) . mapping _Nat

-- | The folder name.
fcName :: Lens' FolderConfiguration FolderName
fcName = lens _fcName (\s a -> s {_fcName = a})

-- | The action to take on the folder contents at the end of the folder configuration period.
fcAction :: Lens' FolderConfiguration RetentionAction
fcAction = lens _fcAction (\s a -> s {_fcAction = a})

instance FromJSON FolderConfiguration where
  parseJSON =
    withObject
      "FolderConfiguration"
      ( \x ->
          FolderConfiguration'
            <$> (x .:? "Period") <*> (x .: "Name") <*> (x .: "Action")
      )

instance Hashable FolderConfiguration

instance NFData FolderConfiguration

instance ToJSON FolderConfiguration where
  toJSON FolderConfiguration' {..} =
    object
      ( catMaybes
          [ ("Period" .=) <$> _fcPeriod,
            Just ("Name" .= _fcName),
            Just ("Action" .= _fcAction)
          ]
      )
