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
-- Module      : Amazonka.TimeStreamQuery.UntagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association of tags from a Timestream query resource.
module Amazonka.TimeStreamQuery.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,

    -- * Response Lenses
    untagResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The Timestream resource that the tags will be removed from. This value
    -- is an Amazon Resource Name (ARN).
    resourceARN :: Prelude.Text,
    -- | A list of tags keys. Existing tags of the resource whose keys are
    -- members of this list will be removed from the Timestream resource.
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
-- 'resourceARN', 'untagResource_resourceARN' - The Timestream resource that the tags will be removed from. This value
-- is an Amazon Resource Name (ARN).
--
-- 'tagKeys', 'untagResource_tagKeys' - A list of tags keys. Existing tags of the resource whose keys are
-- members of this list will be removed from the Timestream resource.
newUntagResource ::
  -- | 'resourceARN'
  Prelude.Text ->
  UntagResource
newUntagResource pResourceARN_ =
  UntagResource'
    { resourceARN = pResourceARN_,
      tagKeys = Prelude.mempty
    }

-- | The Timestream resource that the tags will be removed from. This value
-- is an Amazon Resource Name (ARN).
untagResource_resourceARN :: Lens.Lens' UntagResource Prelude.Text
untagResource_resourceARN = Lens.lens (\UntagResource' {resourceARN} -> resourceARN) (\s@UntagResource' {} a -> s {resourceARN = a} :: UntagResource)

-- | A list of tags keys. Existing tags of the resource whose keys are
-- members of this list will be removed from the Timestream resource.
untagResource_tagKeys :: Lens.Lens' UntagResource [Prelude.Text]
untagResource_tagKeys = Lens.lens (\UntagResource' {tagKeys} -> tagKeys) (\s@UntagResource' {} a -> s {tagKeys = a} :: UntagResource) Prelude.. Lens.coerced

instance Core.AWSRequest UntagResource where
  type
    AWSResponse UntagResource =
      UntagResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagResource where
  hashWithSalt _salt UntagResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.UntagResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Data..= resourceARN),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath UntagResource where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'untagResourceResponse_httpStatus' - The response's http status code.
newUntagResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UntagResourceResponse
newUntagResourceResponse pHttpStatus_ =
  UntagResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
untagResourceResponse_httpStatus :: Lens.Lens' UntagResourceResponse Prelude.Int
untagResourceResponse_httpStatus = Lens.lens (\UntagResourceResponse' {httpStatus} -> httpStatus) (\s@UntagResourceResponse' {} a -> s {httpStatus = a} :: UntagResourceResponse)

instance Prelude.NFData UntagResourceResponse where
  rnf UntagResourceResponse' {..} =
    Prelude.rnf httpStatus
