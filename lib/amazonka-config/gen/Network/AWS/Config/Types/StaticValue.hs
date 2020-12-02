{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.StaticValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StaticValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The static value of the resource.
--
--
--
-- /See:/ 'staticValue' smart constructor.
newtype StaticValue = StaticValue' {_svValues :: [Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StaticValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svValues' - A list of values. For example, the ARN of the assumed role.
staticValue ::
  StaticValue
staticValue = StaticValue' {_svValues = mempty}

-- | A list of values. For example, the ARN of the assumed role.
svValues :: Lens' StaticValue [Text]
svValues = lens _svValues (\s a -> s {_svValues = a}) . _Coerce

instance FromJSON StaticValue where
  parseJSON =
    withObject
      "StaticValue"
      (\x -> StaticValue' <$> (x .:? "Values" .!= mempty))

instance Hashable StaticValue

instance NFData StaticValue

instance ToJSON StaticValue where
  toJSON StaticValue' {..} =
    object (catMaybes [Just ("Values" .= _svValues)])
