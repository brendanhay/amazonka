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
-- Module      : Amazonka.IAM.UntagPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the customer managed policy. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagPolicy
  ( -- * Creating a Request
    UntagPolicy (..),
    newUntagPolicy,

    -- * Request Lenses
    untagPolicy_policyArn,
    untagPolicy_tagKeys,

    -- * Destructuring the Response
    UntagPolicyResponse (..),
    newUntagPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagPolicy' smart constructor.
data UntagPolicy = UntagPolicy'
  { -- | The ARN of the IAM customer managed policy from which you want to remove
    -- tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyArn :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified policy.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'untagPolicy_policyArn' - The ARN of the IAM customer managed policy from which you want to remove
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagPolicy_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified policy.
newUntagPolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  UntagPolicy
newUntagPolicy pPolicyArn_ =
  UntagPolicy'
    { policyArn = pPolicyArn_,
      tagKeys = Prelude.mempty
    }

-- | The ARN of the IAM customer managed policy from which you want to remove
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagPolicy_policyArn :: Lens.Lens' UntagPolicy Prelude.Text
untagPolicy_policyArn = Lens.lens (\UntagPolicy' {policyArn} -> policyArn) (\s@UntagPolicy' {} a -> s {policyArn = a} :: UntagPolicy)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified policy.
untagPolicy_tagKeys :: Lens.Lens' UntagPolicy [Prelude.Text]
untagPolicy_tagKeys = Lens.lens (\UntagPolicy' {tagKeys} -> tagKeys) (\s@UntagPolicy' {} a -> s {tagKeys = a} :: UntagPolicy) Prelude.. Lens.coerced

instance Core.AWSRequest UntagPolicy where
  type AWSResponse UntagPolicy = UntagPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull UntagPolicyResponse'

instance Prelude.Hashable UntagPolicy where
  hashWithSalt _salt UntagPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagPolicy where
  rnf UntagPolicy' {..} =
    Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagPolicy where
  toQuery UntagPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "PolicyArn" Data.=: policyArn,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagPolicyResponse' smart constructor.
data UntagPolicyResponse = UntagPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagPolicyResponse ::
  UntagPolicyResponse
newUntagPolicyResponse = UntagPolicyResponse'

instance Prelude.NFData UntagPolicyResponse where
  rnf _ = ()
