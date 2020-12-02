{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AliasRoutingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AliasRoutingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html traffic-shifting> configuration of a Lambda function alias.
--
--
--
-- /See:/ 'aliasRoutingConfiguration' smart constructor.
newtype AliasRoutingConfiguration = AliasRoutingConfiguration'
  { _arcAdditionalVersionWeights ::
      Maybe (Map Text (Double))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AliasRoutingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arcAdditionalVersionWeights' - The second version, and the percentage of traffic that's routed to it.
aliasRoutingConfiguration ::
  AliasRoutingConfiguration
aliasRoutingConfiguration =
  AliasRoutingConfiguration'
    { _arcAdditionalVersionWeights =
        Nothing
    }

-- | The second version, and the percentage of traffic that's routed to it.
arcAdditionalVersionWeights :: Lens' AliasRoutingConfiguration (HashMap Text (Double))
arcAdditionalVersionWeights = lens _arcAdditionalVersionWeights (\s a -> s {_arcAdditionalVersionWeights = a}) . _Default . _Map

instance FromJSON AliasRoutingConfiguration where
  parseJSON =
    withObject
      "AliasRoutingConfiguration"
      ( \x ->
          AliasRoutingConfiguration'
            <$> (x .:? "AdditionalVersionWeights" .!= mempty)
      )

instance Hashable AliasRoutingConfiguration

instance NFData AliasRoutingConfiguration

instance ToJSON AliasRoutingConfiguration where
  toJSON AliasRoutingConfiguration' {..} =
    object
      ( catMaybes
          [("AdditionalVersionWeights" .=) <$> _arcAdditionalVersionWeights]
      )
