{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.StatusCodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.StatusCodes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html Status Code Definitions> .
--
--
--
-- /See:/ 'statusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { _scStatus2xx :: !(Maybe Int),
    _scStatus3xx :: !(Maybe Int),
    _scStatus4xx :: !(Maybe Int),
    _scStatus5xx :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatusCodes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scStatus2xx' - The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
--
-- * 'scStatus3xx' - The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
--
-- * 'scStatus4xx' - The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
--
-- * 'scStatus5xx' - The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
statusCodes ::
  StatusCodes
statusCodes =
  StatusCodes'
    { _scStatus2xx = Nothing,
      _scStatus3xx = Nothing,
      _scStatus4xx = Nothing,
      _scStatus5xx = Nothing
    }

-- | The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
scStatus2xx :: Lens' StatusCodes (Maybe Int)
scStatus2xx = lens _scStatus2xx (\s a -> s {_scStatus2xx = a})

-- | The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
scStatus3xx :: Lens' StatusCodes (Maybe Int)
scStatus3xx = lens _scStatus3xx (\s a -> s {_scStatus3xx = a})

-- | The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
scStatus4xx :: Lens' StatusCodes (Maybe Int)
scStatus4xx = lens _scStatus4xx (\s a -> s {_scStatus4xx = a})

-- | The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
scStatus5xx :: Lens' StatusCodes (Maybe Int)
scStatus5xx = lens _scStatus5xx (\s a -> s {_scStatus5xx = a})

instance FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      <$> (x .@? "Status2xx")
      <*> (x .@? "Status3xx")
      <*> (x .@? "Status4xx")
      <*> (x .@? "Status5xx")

instance Hashable StatusCodes

instance NFData StatusCodes
