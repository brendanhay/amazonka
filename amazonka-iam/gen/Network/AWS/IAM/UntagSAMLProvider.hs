{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.UntagSAMLProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.IAM.UntagSAMLProvider
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagSAMLProvider' smart constructor.
data UntagSAMLProvider = UntagSAMLProvider'
  { -- | The ARN of the SAML identity provider in IAM from which you want to
    -- remove tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    sAMLProviderArn :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified SAML identity provider.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
untagSAMLProvider_sAMLProviderArn :: Lens.Lens' UntagSAMLProvider Prelude.Text
untagSAMLProvider_sAMLProviderArn = Lens.lens (\UntagSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@UntagSAMLProvider' {} a -> s {sAMLProviderArn = a} :: UntagSAMLProvider)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified SAML identity provider.
untagSAMLProvider_tagKeys :: Lens.Lens' UntagSAMLProvider [Prelude.Text]
untagSAMLProvider_tagKeys = Lens.lens (\UntagSAMLProvider' {tagKeys} -> tagKeys) (\s@UntagSAMLProvider' {} a -> s {tagKeys = a} :: UntagSAMLProvider) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagSAMLProvider where
  type Rs UntagSAMLProvider = UntagSAMLProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UntagSAMLProviderResponse'

instance Prelude.Hashable UntagSAMLProvider

instance Prelude.NFData UntagSAMLProvider

instance Prelude.ToHeaders UntagSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagSAMLProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagSAMLProvider where
  toQuery UntagSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagSAMLProvider" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "SAMLProviderArn" Prelude.=: sAMLProviderArn,
        "TagKeys"
          Prelude.=: Prelude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagSAMLProviderResponse' smart constructor.
data UntagSAMLProviderResponse = UntagSAMLProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagSAMLProviderResponse ::
  UntagSAMLProviderResponse
newUntagSAMLProviderResponse =
  UntagSAMLProviderResponse'

instance Prelude.NFData UntagSAMLProviderResponse
