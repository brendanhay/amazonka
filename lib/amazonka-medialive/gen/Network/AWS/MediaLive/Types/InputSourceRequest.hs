{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for for a PULL type input.
--
-- /See:/ 'inputSourceRequest' smart constructor.
data InputSourceRequest = InputSourceRequest'
  { _isrURL ::
      !(Maybe Text),
    _isrUsername :: !(Maybe Text),
    _isrPasswordParam :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSourceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isrURL' - This represents the customer's source URL where stream is pulled from.
--
-- * 'isrUsername' - The username for the input source.
--
-- * 'isrPasswordParam' - The key used to extract the password from EC2 Parameter store.
inputSourceRequest ::
  InputSourceRequest
inputSourceRequest =
  InputSourceRequest'
    { _isrURL = Nothing,
      _isrUsername = Nothing,
      _isrPasswordParam = Nothing
    }

-- | This represents the customer's source URL where stream is pulled from.
isrURL :: Lens' InputSourceRequest (Maybe Text)
isrURL = lens _isrURL (\s a -> s {_isrURL = a})

-- | The username for the input source.
isrUsername :: Lens' InputSourceRequest (Maybe Text)
isrUsername = lens _isrUsername (\s a -> s {_isrUsername = a})

-- | The key used to extract the password from EC2 Parameter store.
isrPasswordParam :: Lens' InputSourceRequest (Maybe Text)
isrPasswordParam = lens _isrPasswordParam (\s a -> s {_isrPasswordParam = a})

instance Hashable InputSourceRequest

instance NFData InputSourceRequest

instance ToJSON InputSourceRequest where
  toJSON InputSourceRequest' {..} =
    object
      ( catMaybes
          [ ("url" .=) <$> _isrURL,
            ("username" .=) <$> _isrUsername,
            ("passwordParam" .=) <$> _isrPasswordParam
          ]
      )
