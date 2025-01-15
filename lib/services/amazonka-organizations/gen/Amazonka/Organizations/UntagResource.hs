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
-- Module      : Amazonka.Organizations.UntagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes any tags with the specified keys from the specified resource.
--
-- You can attach tags to the following resources in Organizations.
--
-- -   Amazon Web Services account
--
-- -   Organization root
--
-- -   Organizational unit (OU)
--
-- -   Policy (any type)
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceId,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The ID of the resource to remove a tag from.
    --
    -- You can specify any of the following taggable resources.
    --
    -- -   Amazon Web Services account – specify the account ID number.
    --
    -- -   Organizational unit – specify the OU ID that begins with @ou-@ and
    --     looks similar to: @ou-@/@1a2b-34uvwxyz@/@ @
    --
    -- -   Root – specify the root ID that begins with @r-@ and looks similar
    --     to: @r-@/@1a2b@/@ @
    --
    -- -   Policy – specify the policy ID that begins with @p-@ andlooks
    --     similar to: @p-@/@12abcdefg3@/@ @
    resourceId :: Prelude.Text,
    -- | The list of keys for tags to remove from the specified resource.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'untagResource_resourceId' - The ID of the resource to remove a tag from.
--
-- You can specify any of the following taggable resources.
--
-- -   Amazon Web Services account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-@/@1a2b-34uvwxyz@/@ @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-@/@1a2b@/@ @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-@/@12abcdefg3@/@ @
--
-- 'tagKeys', 'untagResource_tagKeys' - The list of keys for tags to remove from the specified resource.
newUntagResource ::
  -- | 'resourceId'
  Prelude.Text ->
  UntagResource
newUntagResource pResourceId_ =
  UntagResource'
    { resourceId = pResourceId_,
      tagKeys = Prelude.mempty
    }

-- | The ID of the resource to remove a tag from.
--
-- You can specify any of the following taggable resources.
--
-- -   Amazon Web Services account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-@/@1a2b-34uvwxyz@/@ @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-@/@1a2b@/@ @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-@/@12abcdefg3@/@ @
untagResource_resourceId :: Lens.Lens' UntagResource Prelude.Text
untagResource_resourceId = Lens.lens (\UntagResource' {resourceId} -> resourceId) (\s@UntagResource' {} a -> s {resourceId = a} :: UntagResource)

-- | The list of keys for tags to remove from the specified resource.
untagResource_tagKeys :: Lens.Lens' UntagResource [Prelude.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Prelude.. Lens.coerced

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UntagResourceResponse'

instance Prelude.Hashable UntagResource where
  hashWithSalt _salt UntagResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf resourceId `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.UntagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagResourceResponse ::
  UntagResourceResponse
newUntagResourceResponse = UntagResourceResponse'

instance Prelude.NFData UntagResourceResponse where
  rnf _ = ()
