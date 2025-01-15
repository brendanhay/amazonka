{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IAM.UntagOpenIDConnectProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified OpenID Connect
-- (OIDC)-compatible identity provider in IAM. For more information about
-- OIDC providers, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc.html About web identity federation>.
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagOpenIDConnectProvider
  ( -- * Creating a Request
    UntagOpenIDConnectProvider (..),
    newUntagOpenIDConnectProvider,

    -- * Request Lenses
    untagOpenIDConnectProvider_openIDConnectProviderArn,
    untagOpenIDConnectProvider_tagKeys,

    -- * Destructuring the Response
    UntagOpenIDConnectProviderResponse (..),
    newUntagOpenIDConnectProviderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagOpenIDConnectProvider' smart constructor.
data UntagOpenIDConnectProvider = UntagOpenIDConnectProvider'
  { -- | The ARN of the OIDC provider in IAM from which you want to remove tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    openIDConnectProviderArn :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified OIDC provider.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'untagOpenIDConnectProvider_openIDConnectProviderArn' - The ARN of the OIDC provider in IAM from which you want to remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagOpenIDConnectProvider_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified OIDC provider.
newUntagOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  UntagOpenIDConnectProvider
newUntagOpenIDConnectProvider
  pOpenIDConnectProviderArn_ =
    UntagOpenIDConnectProvider'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_,
        tagKeys = Prelude.mempty
      }

-- | The ARN of the OIDC provider in IAM from which you want to remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' UntagOpenIDConnectProvider Prelude.Text
untagOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\UntagOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@UntagOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: UntagOpenIDConnectProvider)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified OIDC provider.
untagOpenIDConnectProvider_tagKeys :: Lens.Lens' UntagOpenIDConnectProvider [Prelude.Text]
untagOpenIDConnectProvider_tagKeys = Lens.lens (\UntagOpenIDConnectProvider' {tagKeys} -> tagKeys) (\s@UntagOpenIDConnectProvider' {} a -> s {tagKeys = a} :: UntagOpenIDConnectProvider) Prelude.. Lens.coerced

instance Core.AWSRequest UntagOpenIDConnectProvider where
  type
    AWSResponse UntagOpenIDConnectProvider =
      UntagOpenIDConnectProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      UntagOpenIDConnectProviderResponse'

instance Prelude.Hashable UntagOpenIDConnectProvider where
  hashWithSalt _salt UntagOpenIDConnectProvider' {..} =
    _salt
      `Prelude.hashWithSalt` openIDConnectProviderArn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagOpenIDConnectProvider where
  rnf UntagOpenIDConnectProvider' {..} =
    Prelude.rnf openIDConnectProviderArn `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders UntagOpenIDConnectProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagOpenIDConnectProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagOpenIDConnectProvider where
  toQuery UntagOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagOpenIDConnectProvider" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Data.=: openIDConnectProviderArn,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagOpenIDConnectProviderResponse' smart constructor.
data UntagOpenIDConnectProviderResponse = UntagOpenIDConnectProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagOpenIDConnectProviderResponse ::
  UntagOpenIDConnectProviderResponse
newUntagOpenIDConnectProviderResponse =
  UntagOpenIDConnectProviderResponse'

instance
  Prelude.NFData
    UntagOpenIDConnectProviderResponse
  where
  rnf _ = ()
