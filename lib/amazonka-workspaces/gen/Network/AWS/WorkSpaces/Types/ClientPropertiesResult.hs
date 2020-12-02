{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientPropertiesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ClientPropertiesResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ClientProperties

-- | Information about the Amazon WorkSpaces client.
--
--
--
-- /See:/ 'clientPropertiesResult' smart constructor.
data ClientPropertiesResult = ClientPropertiesResult'
  { _cprResourceId ::
      !(Maybe Text),
    _cprClientProperties ::
      !(Maybe ClientProperties)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClientPropertiesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprResourceId' - The resource identifier, in the form of a directory ID.
--
-- * 'cprClientProperties' - Information about the Amazon WorkSpaces client.
clientPropertiesResult ::
  ClientPropertiesResult
clientPropertiesResult =
  ClientPropertiesResult'
    { _cprResourceId = Nothing,
      _cprClientProperties = Nothing
    }

-- | The resource identifier, in the form of a directory ID.
cprResourceId :: Lens' ClientPropertiesResult (Maybe Text)
cprResourceId = lens _cprResourceId (\s a -> s {_cprResourceId = a})

-- | Information about the Amazon WorkSpaces client.
cprClientProperties :: Lens' ClientPropertiesResult (Maybe ClientProperties)
cprClientProperties = lens _cprClientProperties (\s a -> s {_cprClientProperties = a})

instance FromJSON ClientPropertiesResult where
  parseJSON =
    withObject
      "ClientPropertiesResult"
      ( \x ->
          ClientPropertiesResult'
            <$> (x .:? "ResourceId") <*> (x .:? "ClientProperties")
      )

instance Hashable ClientPropertiesResult

instance NFData ClientPropertiesResult
