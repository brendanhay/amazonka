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
-- Module      : Amazonka.Transfer.UntagResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a key-value pair from a resource, as identified by its Amazon
-- Resource Name (ARN). Resources are users, servers, roles, and other
-- entities.
--
-- No response is returned from this call.
module Amazonka.Transfer.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_arn,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The value of the resource that will have the tag removed. An Amazon
    -- Resource Name (ARN) is an identifier for a specific Amazon Web Services
    -- resource, such as a server, user, or role.
    arn :: Prelude.Text,
    -- | TagKeys are key-value pairs assigned to ARNs that can be used to group
    -- and search for resources by type. This metadata can be attached to
    -- resources for any purpose.
    tagKeys :: Prelude.NonEmpty Prelude.Text
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
-- 'arn', 'untagResource_arn' - The value of the resource that will have the tag removed. An Amazon
-- Resource Name (ARN) is an identifier for a specific Amazon Web Services
-- resource, such as a server, user, or role.
--
-- 'tagKeys', 'untagResource_tagKeys' - TagKeys are key-value pairs assigned to ARNs that can be used to group
-- and search for resources by type. This metadata can be attached to
-- resources for any purpose.
newUntagResource ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  UntagResource
newUntagResource pArn_ pTagKeys_ =
  UntagResource'
    { arn = pArn_,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | The value of the resource that will have the tag removed. An Amazon
-- Resource Name (ARN) is an identifier for a specific Amazon Web Services
-- resource, such as a server, user, or role.
untagResource_arn :: Lens.Lens' UntagResource Prelude.Text
untagResource_arn = Lens.lens (\UntagResource' {arn} -> arn) (\s@UntagResource' {} a -> s {arn = a} :: UntagResource)

-- | TagKeys are key-value pairs assigned to ARNs that can be used to group
-- and search for resources by type. This metadata can be attached to
-- resources for any purpose.
untagResource_tagKeys :: Lens.Lens' UntagResource (Prelude.NonEmpty Prelude.Text)
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
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.UntagResource" ::
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
          [ Prelude.Just ("Arn" Data..= arn),
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
