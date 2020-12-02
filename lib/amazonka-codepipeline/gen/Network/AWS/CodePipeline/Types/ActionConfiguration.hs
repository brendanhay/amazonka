{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an action configuration.
--
--
--
-- /See:/ 'actionConfiguration' smart constructor.
newtype ActionConfiguration = ActionConfiguration'
  { _acConfiguration ::
      Maybe (Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acConfiguration' - The configuration data for the action.
actionConfiguration ::
  ActionConfiguration
actionConfiguration =
  ActionConfiguration' {_acConfiguration = Nothing}

-- | The configuration data for the action.
acConfiguration :: Lens' ActionConfiguration (HashMap Text (Text))
acConfiguration = lens _acConfiguration (\s a -> s {_acConfiguration = a}) . _Default . _Map

instance FromJSON ActionConfiguration where
  parseJSON =
    withObject
      "ActionConfiguration"
      ( \x ->
          ActionConfiguration' <$> (x .:? "configuration" .!= mempty)
      )

instance Hashable ActionConfiguration

instance NFData ActionConfiguration
