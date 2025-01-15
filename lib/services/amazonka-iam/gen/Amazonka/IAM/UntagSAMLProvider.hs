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
-- Module      : Amazonka.IAM.UntagSAMLProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified Security Assertion Markup
-- Language (SAML) identity provider in IAM. For more information about
-- these providers, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc.html About web identity federation>.
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagSAMLProvider
  ( -- * Creating a Request
    UntagSAMLProvider (..),
    newUntagSAMLProvider,

    -- * Request Lenses
    untagSAMLProvider_sAMLProviderArn,
    untagSAMLProvider_tagKeys,

    -- * Destructuring the Response
    UntagSAMLProviderResponse (..),
    newUntagSAMLProviderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagSAMLProvider' smart constructor.
data UntagSAMLProvider = UntagSAMLProvider'
  { -- | The ARN of the SAML identity provider in IAM from which you want to
    -- remove tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    sAMLProviderArn :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified SAML identity provider.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagSAMLProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderArn', 'untagSAMLProvider_sAMLProviderArn' - The ARN of the SAML identity provider in IAM from which you want to
-- remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagSAMLProvider_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified SAML identity provider.
newUntagSAMLProvider ::
  -- | 'sAMLProviderArn'
  Prelude.Text ->
  UntagSAMLProvider
newUntagSAMLProvider pSAMLProviderArn_ =
  UntagSAMLProvider'
    { sAMLProviderArn =
        pSAMLProviderArn_,
      tagKeys = Prelude.mempty
    }

-- | The ARN of the SAML identity provider in IAM from which you want to
-- remove tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagSAMLProvider_sAMLProviderArn :: Lens.Lens' UntagSAMLProvider Prelude.Text
untagSAMLProvider_sAMLProviderArn = Lens.lens (\UntagSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@UntagSAMLProvider' {} a -> s {sAMLProviderArn = a} :: UntagSAMLProvider)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified SAML identity provider.
untagSAMLProvider_tagKeys :: Lens.Lens' UntagSAMLProvider [Prelude.Text]
untagSAMLProvider_tagKeys = Lens.lens (\UntagSAMLProvider' {tagKeys} -> tagKeys) (\s@UntagSAMLProvider' {} a -> s {tagKeys = a} :: UntagSAMLProvider) Prelude.. Lens.coerced

instance Core.AWSRequest UntagSAMLProvider where
  type
    AWSResponse UntagSAMLProvider =
      UntagSAMLProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull UntagSAMLProviderResponse'

instance Prelude.Hashable UntagSAMLProvider where
  hashWithSalt _salt UntagSAMLProvider' {..} =
    _salt
      `Prelude.hashWithSalt` sAMLProviderArn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagSAMLProvider where
  rnf UntagSAMLProvider' {..} =
    Prelude.rnf sAMLProviderArn `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders UntagSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagSAMLProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagSAMLProvider where
  toQuery UntagSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagSAMLProvider" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SAMLProviderArn" Data.=: sAMLProviderArn,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagSAMLProviderResponse' smart constructor.
data UntagSAMLProviderResponse = UntagSAMLProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagSAMLProviderResponse ::
  UntagSAMLProviderResponse
newUntagSAMLProviderResponse =
  UntagSAMLProviderResponse'

instance Prelude.NFData UntagSAMLProviderResponse where
  rnf _ = ()
