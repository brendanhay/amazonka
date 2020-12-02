{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGatewayManagementAPI.Types.Identity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'identity' smart constructor.
data Identity = Identity'
  { _iSourceIP :: !Text,
    _iUserAgent :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Identity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iSourceIP' - The source IP address of the TCP connection making the request to API Gateway.
--
-- * 'iUserAgent' - The User Agent of the API caller.
identity ::
  -- | 'iSourceIP'
  Text ->
  -- | 'iUserAgent'
  Text ->
  Identity
identity pSourceIP_ pUserAgent_ =
  Identity' {_iSourceIP = pSourceIP_, _iUserAgent = pUserAgent_}

-- | The source IP address of the TCP connection making the request to API Gateway.
iSourceIP :: Lens' Identity Text
iSourceIP = lens _iSourceIP (\s a -> s {_iSourceIP = a})

-- | The User Agent of the API caller.
iUserAgent :: Lens' Identity Text
iUserAgent = lens _iUserAgent (\s a -> s {_iUserAgent = a})

instance FromJSON Identity where
  parseJSON =
    withObject
      "Identity"
      (\x -> Identity' <$> (x .: "sourceIp") <*> (x .: "userAgent"))

instance Hashable Identity

instance NFData Identity
