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
-- Module      : Amazonka.StorageGateway.RemoveTagsFromResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified resource. This operation is
-- supported in storage gateways of all types.
module Amazonka.StorageGateway.RemoveTagsFromResource
  ( -- * Creating a Request
    RemoveTagsFromResource (..),
    newRemoveTagsFromResource,

    -- * Request Lenses
    removeTagsFromResource_resourceARN,
    removeTagsFromResource_tagKeys,

    -- * Destructuring the Response
    RemoveTagsFromResourceResponse (..),
    newRemoveTagsFromResourceResponse,

    -- * Response Lenses
    removeTagsFromResourceResponse_resourceARN,
    removeTagsFromResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | RemoveTagsFromResourceInput
--
-- /See:/ 'newRemoveTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
  { -- | The Amazon Resource Name (ARN) of the resource you want to remove the
    -- tags from.
    resourceARN :: Prelude.Text,
    -- | The keys of the tags you want to remove from the specified resource. A
    -- tag is composed of a key-value pair.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'removeTagsFromResource_resourceARN' - The Amazon Resource Name (ARN) of the resource you want to remove the
-- tags from.
--
-- 'tagKeys', 'removeTagsFromResource_tagKeys' - The keys of the tags you want to remove from the specified resource. A
-- tag is composed of a key-value pair.
newRemoveTagsFromResource ::
  -- | 'resourceARN'
  Prelude.Text ->
  RemoveTagsFromResource
newRemoveTagsFromResource pResourceARN_ =
  RemoveTagsFromResource'
    { resourceARN =
        pResourceARN_,
      tagKeys = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource you want to remove the
-- tags from.
removeTagsFromResource_resourceARN :: Lens.Lens' RemoveTagsFromResource Prelude.Text
removeTagsFromResource_resourceARN = Lens.lens (\RemoveTagsFromResource' {resourceARN} -> resourceARN) (\s@RemoveTagsFromResource' {} a -> s {resourceARN = a} :: RemoveTagsFromResource)

-- | The keys of the tags you want to remove from the specified resource. A
-- tag is composed of a key-value pair.
removeTagsFromResource_tagKeys :: Lens.Lens' RemoveTagsFromResource [Prelude.Text]
removeTagsFromResource_tagKeys = Lens.lens (\RemoveTagsFromResource' {tagKeys} -> tagKeys) (\s@RemoveTagsFromResource' {} a -> s {tagKeys = a} :: RemoveTagsFromResource) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromResource where
  type
    AWSResponse RemoveTagsFromResource =
      RemoveTagsFromResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveTagsFromResourceResponse'
            Prelude.<$> (x Data..?> "ResourceARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTagsFromResource where
  hashWithSalt _salt RemoveTagsFromResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTagsFromResource where
  rnf RemoveTagsFromResource' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.RemoveTagsFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTagsFromResource where
  toJSON RemoveTagsFromResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Data..= resourceARN),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTagsFromResource where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTagsFromResource where
  toQuery = Prelude.const Prelude.mempty

-- | RemoveTagsFromResourceOutput
--
-- /See:/ 'newRemoveTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse = RemoveTagsFromResourceResponse'
  { -- | The Amazon Resource Name (ARN) of the resource that the tags were
    -- removed from.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'removeTagsFromResourceResponse_resourceARN' - The Amazon Resource Name (ARN) of the resource that the tags were
-- removed from.
--
-- 'httpStatus', 'removeTagsFromResourceResponse_httpStatus' - The response's http status code.
newRemoveTagsFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsFromResourceResponse
newRemoveTagsFromResourceResponse pHttpStatus_ =
  RemoveTagsFromResourceResponse'
    { resourceARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource that the tags were
-- removed from.
removeTagsFromResourceResponse_resourceARN :: Lens.Lens' RemoveTagsFromResourceResponse (Prelude.Maybe Prelude.Text)
removeTagsFromResourceResponse_resourceARN = Lens.lens (\RemoveTagsFromResourceResponse' {resourceARN} -> resourceARN) (\s@RemoveTagsFromResourceResponse' {} a -> s {resourceARN = a} :: RemoveTagsFromResourceResponse)

-- | The response's http status code.
removeTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveTagsFromResourceResponse Prelude.Int
removeTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveTagsFromResourceResponse)

instance
  Prelude.NFData
    RemoveTagsFromResourceResponse
  where
  rnf RemoveTagsFromResourceResponse' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf httpStatus
