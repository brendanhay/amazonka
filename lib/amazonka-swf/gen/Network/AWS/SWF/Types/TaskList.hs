{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TaskList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TaskList where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a task list.
--
--
--
-- /See:/ 'taskList' smart constructor.
newtype TaskList = TaskList' {_tlName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlName' - The name of the task list.
taskList ::
  -- | 'tlName'
  Text ->
  TaskList
taskList pName_ = TaskList' {_tlName = pName_}

-- | The name of the task list.
tlName :: Lens' TaskList Text
tlName = lens _tlName (\s a -> s {_tlName = a})

instance FromJSON TaskList where
  parseJSON =
    withObject "TaskList" (\x -> TaskList' <$> (x .: "name"))

instance Hashable TaskList

instance NFData TaskList

instance ToJSON TaskList where
  toJSON TaskList' {..} =
    object (catMaybes [Just ("name" .= _tlName)])
