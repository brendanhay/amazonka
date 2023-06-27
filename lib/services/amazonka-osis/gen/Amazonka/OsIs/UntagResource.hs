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
-- Module      : Amazonka.OsIs.UntagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from an OpenSearch Ingestion pipeline. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/tag-pipeline.html Tagging Amazon OpenSearch Ingestion pipelines>.
module Amazonka.OsIs.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_arn,
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
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The Amazon Resource Name (ARN) of the pipeline to remove tags from.
    arn :: Prelude.Text,
    -- | The tag keys to remove.
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
-- 'arn', 'untagResource_arn' - The Amazon Resource Name (ARN) of the pipeline to remove tags from.
--
-- 'tagKeys', 'untagResource_tagKeys' - The tag keys to remove.
newUntagResource ::
  -- | 'arn'
  Prelude.Text ->
  UntagResource
newUntagResource pArn_ =
  UntagResource'
    { arn = pArn_,
      tagKeys = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the pipeline to remove tags from.
untagResource_arn :: Lens.Lens' UntagResource Prelude.Text
untagResource_arn = Lens.lens (\UntagResource' {arn} -> arn) (\s@UntagResource' {} a -> s {arn = a} :: UntagResource)

-- | The tag keys to remove.
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
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UntagResource where
  toJSON UntagResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TagKeys" Data..= tagKeys)]
      )

instance Data.ToPath UntagResource where
  toPath =
    Prelude.const "/2022-01-01/osis/untagResource/"

instance Data.ToQuery UntagResource where
  toQuery UntagResource' {..} =
    Prelude.mconcat ["arn" Data.=: arn]

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
