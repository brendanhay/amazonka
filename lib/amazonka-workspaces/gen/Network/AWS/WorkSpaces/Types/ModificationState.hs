{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ModificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationState where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationStateEnum

-- | Describes a WorkSpace modification.
--
--
--
-- /See:/ 'modificationState' smart constructor.
data ModificationState = ModificationState'
  { _msState ::
      !(Maybe ModificationStateEnum),
    _msResource :: !(Maybe ModificationResourceEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModificationState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msState' - The modification state.
--
-- * 'msResource' - The resource.
modificationState ::
  ModificationState
modificationState =
  ModificationState' {_msState = Nothing, _msResource = Nothing}

-- | The modification state.
msState :: Lens' ModificationState (Maybe ModificationStateEnum)
msState = lens _msState (\s a -> s {_msState = a})

-- | The resource.
msResource :: Lens' ModificationState (Maybe ModificationResourceEnum)
msResource = lens _msResource (\s a -> s {_msResource = a})

instance FromJSON ModificationState where
  parseJSON =
    withObject
      "ModificationState"
      ( \x ->
          ModificationState' <$> (x .:? "State") <*> (x .:? "Resource")
      )

instance Hashable ModificationState

instance NFData ModificationState
