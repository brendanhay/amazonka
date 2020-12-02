{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a routing profile.
--
--
--
-- /See:/ 'routingProfileSummary' smart constructor.
data RoutingProfileSummary = RoutingProfileSummary'
  { _rpsARN ::
      !(Maybe Text),
    _rpsName :: !(Maybe Text),
    _rpsId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoutingProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsARN' - The Amazon Resource Name (ARN) of the routing profile.
--
-- * 'rpsName' - The name of the routing profile.
--
-- * 'rpsId' - The identifier of the routing profile.
routingProfileSummary ::
  RoutingProfileSummary
routingProfileSummary =
  RoutingProfileSummary'
    { _rpsARN = Nothing,
      _rpsName = Nothing,
      _rpsId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
rpsARN :: Lens' RoutingProfileSummary (Maybe Text)
rpsARN = lens _rpsARN (\s a -> s {_rpsARN = a})

-- | The name of the routing profile.
rpsName :: Lens' RoutingProfileSummary (Maybe Text)
rpsName = lens _rpsName (\s a -> s {_rpsName = a})

-- | The identifier of the routing profile.
rpsId :: Lens' RoutingProfileSummary (Maybe Text)
rpsId = lens _rpsId (\s a -> s {_rpsId = a})

instance FromJSON RoutingProfileSummary where
  parseJSON =
    withObject
      "RoutingProfileSummary"
      ( \x ->
          RoutingProfileSummary'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id")
      )

instance Hashable RoutingProfileSummary

instance NFData RoutingProfileSummary
