{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointResponse
import Network.AWS.Prelude

-- | Provides information about all the endpoints that are associated with a user ID.
--
--
--
-- /See:/ 'endpointsResponse' smart constructor.
newtype EndpointsResponse = EndpointsResponse'
  { _eItem ::
      [EndpointResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eItem' - An array of responses, one for each endpoint that's associated with the user ID.
endpointsResponse ::
  EndpointsResponse
endpointsResponse = EndpointsResponse' {_eItem = mempty}

-- | An array of responses, one for each endpoint that's associated with the user ID.
eItem :: Lens' EndpointsResponse [EndpointResponse]
eItem = lens _eItem (\s a -> s {_eItem = a}) . _Coerce

instance FromJSON EndpointsResponse where
  parseJSON =
    withObject
      "EndpointsResponse"
      (\x -> EndpointsResponse' <$> (x .:? "Item" .!= mempty))

instance Hashable EndpointsResponse

instance NFData EndpointsResponse
