{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.InputContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.InputContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The name of a context that must be active for an intent to be selected by Amazon Lex.
--
--
--
-- /See:/ 'inputContext' smart constructor.
newtype InputContext = InputContext' {_icName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icName' - The name of the context.
inputContext ::
  -- | 'icName'
  Text ->
  InputContext
inputContext pName_ = InputContext' {_icName = pName_}

-- | The name of the context.
icName :: Lens' InputContext Text
icName = lens _icName (\s a -> s {_icName = a})

instance FromJSON InputContext where
  parseJSON =
    withObject
      "InputContext"
      (\x -> InputContext' <$> (x .: "name"))

instance Hashable InputContext

instance NFData InputContext

instance ToJSON InputContext where
  toJSON InputContext' {..} =
    object (catMaybes [Just ("name" .= _icName)])
