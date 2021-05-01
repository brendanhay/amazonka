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
-- Module      : Network.AWS.IAM.UntagOpenIDConnectProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.IAM.UntagOpenIDConnectProvider
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagOpenIDConnectProvider' smart constructor.
data UntagOpenIDConnectProvider = UntagOpenIDConnectProvider'
  { -- | The ARN of the OIDC provider in IAM from which you want to remove tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    openIDConnectProviderArn :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified OIDC provider.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
untagOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' UntagOpenIDConnectProvider Prelude.Text
untagOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\UntagOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@UntagOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: UntagOpenIDConnectProvider)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified OIDC provider.
untagOpenIDConnectProvider_tagKeys :: Lens.Lens' UntagOpenIDConnectProvider [Prelude.Text]
untagOpenIDConnectProvider_tagKeys = Lens.lens (\UntagOpenIDConnectProvider' {tagKeys} -> tagKeys) (\s@UntagOpenIDConnectProvider' {} a -> s {tagKeys = a} :: UntagOpenIDConnectProvider) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    UntagOpenIDConnectProvider
  where
  type
    Rs UntagOpenIDConnectProvider =
      UntagOpenIDConnectProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UntagOpenIDConnectProviderResponse'

instance Prelude.Hashable UntagOpenIDConnectProvider

instance Prelude.NFData UntagOpenIDConnectProvider

instance Prelude.ToHeaders UntagOpenIDConnectProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagOpenIDConnectProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagOpenIDConnectProvider where
  toQuery UntagOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagOpenIDConnectProvider" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Prelude.=: openIDConnectProviderArn,
        "TagKeys"
          Prelude.=: Prelude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagOpenIDConnectProviderResponse' smart constructor.
data UntagOpenIDConnectProviderResponse = UntagOpenIDConnectProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
