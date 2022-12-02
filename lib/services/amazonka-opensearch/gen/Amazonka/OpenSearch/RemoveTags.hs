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
-- Module      : Amazonka.OpenSearch.RemoveTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified set of tags from an Amazon OpenSearch Service
-- domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/managedomains.html#managedomains-awsresorcetagging Tagging Amazon OpenSearch Service domains>.
module Amazonka.OpenSearch.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_arn,
    removeTags_tagKeys,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @RemoveTags@ operation.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The Amazon Resource Name (ARN) of the domain from which you want to
    -- delete the specified tags.
    arn :: Prelude.Text,
    -- | The list of tag keys to remove from the domain.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'removeTags_arn' - The Amazon Resource Name (ARN) of the domain from which you want to
-- delete the specified tags.
--
-- 'tagKeys', 'removeTags_tagKeys' - The list of tag keys to remove from the domain.
newRemoveTags ::
  -- | 'arn'
  Prelude.Text ->
  RemoveTags
newRemoveTags pARN_ =
  RemoveTags' {arn = pARN_, tagKeys = Prelude.mempty}

-- | The Amazon Resource Name (ARN) of the domain from which you want to
-- delete the specified tags.
removeTags_arn :: Lens.Lens' RemoveTags Prelude.Text
removeTags_arn = Lens.lens (\RemoveTags' {arn} -> arn) (\s@RemoveTags' {} a -> s {arn = a} :: RemoveTags)

-- | The list of tag keys to remove from the domain.
removeTags_tagKeys :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_tagKeys = Lens.lens (\RemoveTags' {tagKeys} -> tagKeys) (\s@RemoveTags' {} a -> s {tagKeys = a} :: RemoveTags) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull RemoveTagsResponse'

instance Prelude.Hashable RemoveTags where
  hashWithSalt _salt RemoveTags' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTags where
  rnf RemoveTags' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ARN" Data..= arn),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTags where
  toPath = Prelude.const "/2021-01-01/tags-removal"

instance Data.ToQuery RemoveTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsResponse ::
  RemoveTagsResponse
newRemoveTagsResponse = RemoveTagsResponse'

instance Prelude.NFData RemoveTagsResponse where
  rnf _ = ()
