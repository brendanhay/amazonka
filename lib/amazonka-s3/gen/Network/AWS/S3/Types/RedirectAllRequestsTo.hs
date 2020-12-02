{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RedirectAllRequestsTo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RedirectAllRequestsTo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Protocol

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
--
--
-- /See:/ 'redirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
  { _rartProtocol ::
      !(Maybe Protocol),
    _rartHostName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedirectAllRequestsTo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rartProtocol' - Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- * 'rartHostName' - Name of the host where requests are redirected.
redirectAllRequestsTo ::
  -- | 'rartHostName'
  Text ->
  RedirectAllRequestsTo
redirectAllRequestsTo pHostName_ =
  RedirectAllRequestsTo'
    { _rartProtocol = Nothing,
      _rartHostName = pHostName_
    }

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\s a -> s {_rartProtocol = a})

-- | Name of the host where requests are redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\s a -> s {_rartHostName = a})

instance FromXML RedirectAllRequestsTo where
  parseXML x =
    RedirectAllRequestsTo'
      <$> (x .@? "Protocol") <*> (x .@ "HostName")

instance Hashable RedirectAllRequestsTo

instance NFData RedirectAllRequestsTo

instance ToXML RedirectAllRequestsTo where
  toXML RedirectAllRequestsTo' {..} =
    mconcat
      ["Protocol" @= _rartProtocol, "HostName" @= _rartHostName]
