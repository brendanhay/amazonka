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
-- Module      : Amazonka.Config.UntagResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes specified tags from a resource.
module Amazonka.Config.UntagResource
  ( -- * Creating a Request
    UntagResource (..),
    newUntagResource,

    -- * Request Lenses
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- * Destructuring the Response
    UntagResourceResponse (..),
    newUntagResourceResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The Amazon Resource Name (ARN) that identifies the resource for which to
    -- list the tags. Currently, the supported resources are @ConfigRule@,
    -- @ConfigurationAggregator@ and @AggregatorAuthorization@.
    resourceArn :: Prelude.Text,
    -- | The keys of the tags to be removed.
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
-- 'resourceArn', 'untagResource_resourceArn' - The Amazon Resource Name (ARN) that identifies the resource for which to
-- list the tags. Currently, the supported resources are @ConfigRule@,
-- @ConfigurationAggregator@ and @AggregatorAuthorization@.
--
-- 'tagKeys', 'untagResource_tagKeys' - The keys of the tags to be removed.
newUntagResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  UntagResource
newUntagResource pResourceArn_ pTagKeys_ =
  UntagResource'
    { resourceArn = pResourceArn_,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | The Amazon Resource Name (ARN) that identifies the resource for which to
-- list the tags. Currently, the supported resources are @ConfigRule@,
-- @ConfigurationAggregator@ and @AggregatorAuthorization@.
untagResource_resourceArn :: Lens.Lens' UntagResource Prelude.Text
untagResource_resourceArn = Lens.lens (\UntagResource' {resourceArn} -> resourceArn) (\s@UntagResource' {} a -> s {resourceArn = a} :: UntagResource)

-- | The keys of the tags to be removed.
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
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagResource where
  rnf UntagResource' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.UntagResource" ::
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
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
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
