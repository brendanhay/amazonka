{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserData where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the user data for an instance.
--
--
--
-- /See:/ 'userData' smart constructor.
newtype UserData = UserData' {_udData :: Maybe Text}
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udData' - The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
userData ::
  UserData
userData = UserData' {_udData = Nothing}

-- | The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
udData :: Lens' UserData (Maybe Text)
udData = lens _udData (\s a -> s {_udData = a})

instance Hashable UserData

instance NFData UserData

instance ToQuery UserData where
  toQuery UserData' {..} = mconcat ["Data" =: _udData]
