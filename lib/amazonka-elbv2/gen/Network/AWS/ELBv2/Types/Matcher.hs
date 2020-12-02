{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Matcher
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Matcher where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The codes to use when checking for a successful response from a target. If the protocol version is gRPC, these are gRPC codes. Otherwise, these are HTTP codes.
--
--
--
-- /See:/ 'matcher' smart constructor.
data Matcher = Matcher'
  { _mHTTPCode :: !(Maybe Text),
    _mGrpcCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Matcher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mHTTPCode' - For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers and Gateway Load Balancers, this must be "200–399".
--
-- * 'mGrpcCode' - You can specify values between 0 and 99. You can specify multiple values (for example, "0,1") or a range of values (for example, "0-5"). The default value is 12.
matcher ::
  Matcher
matcher = Matcher' {_mHTTPCode = Nothing, _mGrpcCode = Nothing}

-- | For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers and Gateway Load Balancers, this must be "200–399".
mHTTPCode :: Lens' Matcher (Maybe Text)
mHTTPCode = lens _mHTTPCode (\s a -> s {_mHTTPCode = a})

-- | You can specify values between 0 and 99. You can specify multiple values (for example, "0,1") or a range of values (for example, "0-5"). The default value is 12.
mGrpcCode :: Lens' Matcher (Maybe Text)
mGrpcCode = lens _mGrpcCode (\s a -> s {_mGrpcCode = a})

instance FromXML Matcher where
  parseXML x = Matcher' <$> (x .@? "HttpCode") <*> (x .@? "GrpcCode")

instance Hashable Matcher

instance NFData Matcher

instance ToQuery Matcher where
  toQuery Matcher' {..} =
    mconcat ["HttpCode" =: _mHTTPCode, "GrpcCode" =: _mGrpcCode]
