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
-- Module      : Amazonka.Inspector.SetTagsForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets tags (key and value pairs) to the assessment template that is
-- specified by the ARN of the assessment template.
module Amazonka.Inspector.SetTagsForResource
  ( -- * Creating a Request
    SetTagsForResource (..),
    newSetTagsForResource,

    -- * Request Lenses
    setTagsForResource_tags,
    setTagsForResource_resourceArn,

    -- * Destructuring the Response
    SetTagsForResourceResponse (..),
    newSetTagsForResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetTagsForResource' smart constructor.
data SetTagsForResource = SetTagsForResource'
  { -- | A collection of key and value pairs that you want to set to the
    -- assessment template.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the assessment template that you want to set tags to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'setTagsForResource_tags' - A collection of key and value pairs that you want to set to the
-- assessment template.
--
-- 'resourceArn', 'setTagsForResource_resourceArn' - The ARN of the assessment template that you want to set tags to.
newSetTagsForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  SetTagsForResource
newSetTagsForResource pResourceArn_ =
  SetTagsForResource'
    { tags = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | A collection of key and value pairs that you want to set to the
-- assessment template.
setTagsForResource_tags :: Lens.Lens' SetTagsForResource (Prelude.Maybe [Tag])
setTagsForResource_tags = Lens.lens (\SetTagsForResource' {tags} -> tags) (\s@SetTagsForResource' {} a -> s {tags = a} :: SetTagsForResource) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the assessment template that you want to set tags to.
setTagsForResource_resourceArn :: Lens.Lens' SetTagsForResource Prelude.Text
setTagsForResource_resourceArn = Lens.lens (\SetTagsForResource' {resourceArn} -> resourceArn) (\s@SetTagsForResource' {} a -> s {resourceArn = a} :: SetTagsForResource)

instance Core.AWSRequest SetTagsForResource where
  type
    AWSResponse SetTagsForResource =
      SetTagsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull SetTagsForResourceResponse'

instance Prelude.Hashable SetTagsForResource where
  hashWithSalt _salt SetTagsForResource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData SetTagsForResource where
  rnf SetTagsForResource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceArn

instance Core.ToHeaders SetTagsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.SetTagsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SetTagsForResource where
  toJSON SetTagsForResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("resourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath SetTagsForResource where
  toPath = Prelude.const "/"

instance Core.ToQuery SetTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetTagsForResourceResponse' smart constructor.
data SetTagsForResourceResponse = SetTagsForResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetTagsForResourceResponse ::
  SetTagsForResourceResponse
newSetTagsForResourceResponse =
  SetTagsForResourceResponse'

instance Prelude.NFData SetTagsForResourceResponse where
  rnf _ = ()
