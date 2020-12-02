{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Redirect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Redirect where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Protocol

-- | Specifies how requests are redirected. In the event of an error, you can specify a different error code to return.
--
--
--
-- /See:/ 'redirect' smart constructor.
data Redirect = Redirect'
  { _rHostName :: !(Maybe Text),
    _rProtocol :: !(Maybe Protocol),
    _rHTTPRedirectCode :: !(Maybe Text),
    _rReplaceKeyWith :: !(Maybe Text),
    _rReplaceKeyPrefixWith :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Redirect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHostName' - The host name to use in the redirect request.
--
-- * 'rProtocol' - Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- * 'rHTTPRedirectCode' - The HTTP redirect code to use on the response. Not required if one of the siblings is present.
--
-- * 'rReplaceKeyWith' - The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- * 'rReplaceKeyPrefixWith' - The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
redirect ::
  Redirect
redirect =
  Redirect'
    { _rHostName = Nothing,
      _rProtocol = Nothing,
      _rHTTPRedirectCode = Nothing,
      _rReplaceKeyWith = Nothing,
      _rReplaceKeyPrefixWith = Nothing
    }

-- | The host name to use in the redirect request.
rHostName :: Lens' Redirect (Maybe Text)
rHostName = lens _rHostName (\s a -> s {_rHostName = a})

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
rProtocol :: Lens' Redirect (Maybe Protocol)
rProtocol = lens _rProtocol (\s a -> s {_rProtocol = a})

-- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
rHTTPRedirectCode :: Lens' Redirect (Maybe Text)
rHTTPRedirectCode = lens _rHTTPRedirectCode (\s a -> s {_rHTTPRedirectCode = a})

-- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
rReplaceKeyWith :: Lens' Redirect (Maybe Text)
rReplaceKeyWith = lens _rReplaceKeyWith (\s a -> s {_rReplaceKeyWith = a})

-- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
rReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
rReplaceKeyPrefixWith = lens _rReplaceKeyPrefixWith (\s a -> s {_rReplaceKeyPrefixWith = a})

instance FromXML Redirect where
  parseXML x =
    Redirect'
      <$> (x .@? "HostName")
      <*> (x .@? "Protocol")
      <*> (x .@? "HttpRedirectCode")
      <*> (x .@? "ReplaceKeyWith")
      <*> (x .@? "ReplaceKeyPrefixWith")

instance Hashable Redirect

instance NFData Redirect

instance ToXML Redirect where
  toXML Redirect' {..} =
    mconcat
      [ "HostName" @= _rHostName,
        "Protocol" @= _rProtocol,
        "HttpRedirectCode" @= _rHTTPRedirectCode,
        "ReplaceKeyWith" @= _rReplaceKeyWith,
        "ReplaceKeyPrefixWith" @= _rReplaceKeyPrefixWith
      ]
