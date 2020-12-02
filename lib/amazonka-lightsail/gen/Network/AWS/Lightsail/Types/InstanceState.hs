{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceState where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the virtual private server (or /instance/ ) status.
--
--
--
-- /See:/ 'instanceState' smart constructor.
data InstanceState = InstanceState'
  { _isName :: !(Maybe Text),
    _isCode :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isName' - The state of the instance (e.g., @running@ or @pending@ ).
--
-- * 'isCode' - The status code for the instance.
instanceState ::
  InstanceState
instanceState =
  InstanceState' {_isName = Nothing, _isCode = Nothing}

-- | The state of the instance (e.g., @running@ or @pending@ ).
isName :: Lens' InstanceState (Maybe Text)
isName = lens _isName (\s a -> s {_isName = a})

-- | The status code for the instance.
isCode :: Lens' InstanceState (Maybe Int)
isCode = lens _isCode (\s a -> s {_isCode = a})

instance FromJSON InstanceState where
  parseJSON =
    withObject
      "InstanceState"
      (\x -> InstanceState' <$> (x .:? "name") <*> (x .:? "code"))

instance Hashable InstanceState

instance NFData InstanceState
