{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3.Types.Redirect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Redirect where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Protocol

-- | Specifies how requests are redirected. In the event of an error, you can
-- specify a different error code to return.
--
-- /See:/ 'newRedirect' smart constructor.
data Redirect = Redirect'
  { -- | The host name to use in the redirect request.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | The object key prefix to use in the redirect request. For example, to
    -- redirect requests for all pages with prefix @docs\/@ (objects in the
    -- @docs\/@ folder) to @documents\/@, you can set a condition block with
    -- @KeyPrefixEquals@ set to @docs\/@ and in the Redirect set
    -- @ReplaceKeyPrefixWith@ to @\/documents@. Not required if one of the
    -- siblings is present. Can be present only if @ReplaceKeyWith@ is not
    -- provided.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    replaceKeyPrefixWith :: Prelude.Maybe Prelude.Text,
    -- | The specific object key to use in the redirect request. For example,
    -- redirect request to @error.html@. Not required if one of the siblings is
    -- present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    replaceKeyWith :: Prelude.Maybe Prelude.Text,
    -- | Protocol to use when redirecting requests. The default is the protocol
    -- that is used in the original request.
    protocol :: Prelude.Maybe Protocol,
    -- | The HTTP redirect code to use on the response. Not required if one of
    -- the siblings is present.
    httpRedirectCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Redirect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostName', 'redirect_hostName' - The host name to use in the redirect request.
--
-- 'replaceKeyPrefixWith', 'redirect_replaceKeyPrefixWith' - The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix @docs\/@ (objects in the
-- @docs\/@ folder) to @documents\/@, you can set a condition block with
-- @KeyPrefixEquals@ set to @docs\/@ and in the Redirect set
-- @ReplaceKeyPrefixWith@ to @\/documents@. Not required if one of the
-- siblings is present. Can be present only if @ReplaceKeyWith@ is not
-- provided.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'replaceKeyWith', 'redirect_replaceKeyWith' - The specific object key to use in the redirect request. For example,
-- redirect request to @error.html@. Not required if one of the siblings is
-- present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'protocol', 'redirect_protocol' - Protocol to use when redirecting requests. The default is the protocol
-- that is used in the original request.
--
-- 'httpRedirectCode', 'redirect_httpRedirectCode' - The HTTP redirect code to use on the response. Not required if one of
-- the siblings is present.
newRedirect ::
  Redirect
newRedirect =
  Redirect'
    { hostName = Prelude.Nothing,
      replaceKeyPrefixWith = Prelude.Nothing,
      replaceKeyWith = Prelude.Nothing,
      protocol = Prelude.Nothing,
      httpRedirectCode = Prelude.Nothing
    }

-- | The host name to use in the redirect request.
redirect_hostName :: Lens.Lens' Redirect (Prelude.Maybe Prelude.Text)
redirect_hostName = Lens.lens (\Redirect' {hostName} -> hostName) (\s@Redirect' {} a -> s {hostName = a} :: Redirect)

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix @docs\/@ (objects in the
-- @docs\/@ folder) to @documents\/@, you can set a condition block with
-- @KeyPrefixEquals@ set to @docs\/@ and in the Redirect set
-- @ReplaceKeyPrefixWith@ to @\/documents@. Not required if one of the
-- siblings is present. Can be present only if @ReplaceKeyWith@ is not
-- provided.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
redirect_replaceKeyPrefixWith :: Lens.Lens' Redirect (Prelude.Maybe Prelude.Text)
redirect_replaceKeyPrefixWith = Lens.lens (\Redirect' {replaceKeyPrefixWith} -> replaceKeyPrefixWith) (\s@Redirect' {} a -> s {replaceKeyPrefixWith = a} :: Redirect)

-- | The specific object key to use in the redirect request. For example,
-- redirect request to @error.html@. Not required if one of the siblings is
-- present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
redirect_replaceKeyWith :: Lens.Lens' Redirect (Prelude.Maybe Prelude.Text)
redirect_replaceKeyWith = Lens.lens (\Redirect' {replaceKeyWith} -> replaceKeyWith) (\s@Redirect' {} a -> s {replaceKeyWith = a} :: Redirect)

-- | Protocol to use when redirecting requests. The default is the protocol
-- that is used in the original request.
redirect_protocol :: Lens.Lens' Redirect (Prelude.Maybe Protocol)
redirect_protocol = Lens.lens (\Redirect' {protocol} -> protocol) (\s@Redirect' {} a -> s {protocol = a} :: Redirect)

-- | The HTTP redirect code to use on the response. Not required if one of
-- the siblings is present.
redirect_httpRedirectCode :: Lens.Lens' Redirect (Prelude.Maybe Prelude.Text)
redirect_httpRedirectCode = Lens.lens (\Redirect' {httpRedirectCode} -> httpRedirectCode) (\s@Redirect' {} a -> s {httpRedirectCode = a} :: Redirect)

instance Core.FromXML Redirect where
  parseXML x =
    Redirect'
      Prelude.<$> (x Core..@? "HostName")
      Prelude.<*> (x Core..@? "ReplaceKeyPrefixWith")
      Prelude.<*> (x Core..@? "ReplaceKeyWith")
      Prelude.<*> (x Core..@? "Protocol")
      Prelude.<*> (x Core..@? "HttpRedirectCode")

instance Prelude.Hashable Redirect where
  hashWithSalt _salt Redirect' {..} =
    _salt `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` replaceKeyPrefixWith
      `Prelude.hashWithSalt` replaceKeyWith
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` httpRedirectCode

instance Prelude.NFData Redirect where
  rnf Redirect' {..} =
    Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf replaceKeyPrefixWith
      `Prelude.seq` Prelude.rnf replaceKeyWith
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf httpRedirectCode

instance Core.ToXML Redirect where
  toXML Redirect' {..} =
    Prelude.mconcat
      [ "HostName" Core.@= hostName,
        "ReplaceKeyPrefixWith" Core.@= replaceKeyPrefixWith,
        "ReplaceKeyWith" Core.@= replaceKeyWith,
        "Protocol" Core.@= protocol,
        "HttpRedirectCode" Core.@= httpRedirectCode
      ]
