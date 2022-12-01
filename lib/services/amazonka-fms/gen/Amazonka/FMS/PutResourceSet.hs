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
-- Module      : Amazonka.FMS.PutResourceSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the resource set.
--
-- An Firewall Manager resource set defines the resources to import into an
-- Firewall Manager policy from another Amazon Web Services service.
module Amazonka.FMS.PutResourceSet
  ( -- * Creating a Request
    PutResourceSet (..),
    newPutResourceSet,

    -- * Request Lenses
    putResourceSet_tagList,
    putResourceSet_resourceSet,

    -- * Destructuring the Response
    PutResourceSetResponse (..),
    newPutResourceSetResponse,

    -- * Response Lenses
    putResourceSetResponse_httpStatus,
    putResourceSetResponse_resourceSet,
    putResourceSetResponse_resourceSetArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourceSet' smart constructor.
data PutResourceSet = PutResourceSet'
  { -- | Retrieves the tags associated with the specified resource set. Tags are
    -- key:value pairs that you can use to categorize and manage your
    -- resources, for purposes like billing. For example, you might set the tag
    -- key to \"customer\" and the value to the customer name or ID. You can
    -- specify one or more tags to add to each Amazon Web Services resource, up
    -- to 50 tags for a resource.
    tagList :: Prelude.Maybe [Tag],
    -- | Details about the resource set to be created or updated.>
    resourceSet :: ResourceSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'putResourceSet_tagList' - Retrieves the tags associated with the specified resource set. Tags are
-- key:value pairs that you can use to categorize and manage your
-- resources, for purposes like billing. For example, you might set the tag
-- key to \"customer\" and the value to the customer name or ID. You can
-- specify one or more tags to add to each Amazon Web Services resource, up
-- to 50 tags for a resource.
--
-- 'resourceSet', 'putResourceSet_resourceSet' - Details about the resource set to be created or updated.>
newPutResourceSet ::
  -- | 'resourceSet'
  ResourceSet ->
  PutResourceSet
newPutResourceSet pResourceSet_ =
  PutResourceSet'
    { tagList = Prelude.Nothing,
      resourceSet = pResourceSet_
    }

-- | Retrieves the tags associated with the specified resource set. Tags are
-- key:value pairs that you can use to categorize and manage your
-- resources, for purposes like billing. For example, you might set the tag
-- key to \"customer\" and the value to the customer name or ID. You can
-- specify one or more tags to add to each Amazon Web Services resource, up
-- to 50 tags for a resource.
putResourceSet_tagList :: Lens.Lens' PutResourceSet (Prelude.Maybe [Tag])
putResourceSet_tagList = Lens.lens (\PutResourceSet' {tagList} -> tagList) (\s@PutResourceSet' {} a -> s {tagList = a} :: PutResourceSet) Prelude.. Lens.mapping Lens.coerced

-- | Details about the resource set to be created or updated.>
putResourceSet_resourceSet :: Lens.Lens' PutResourceSet ResourceSet
putResourceSet_resourceSet = Lens.lens (\PutResourceSet' {resourceSet} -> resourceSet) (\s@PutResourceSet' {} a -> s {resourceSet = a} :: PutResourceSet)

instance Core.AWSRequest PutResourceSet where
  type
    AWSResponse PutResourceSet =
      PutResourceSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourceSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ResourceSet")
            Prelude.<*> (x Core..:> "ResourceSetArn")
      )

instance Prelude.Hashable PutResourceSet where
  hashWithSalt _salt PutResourceSet' {..} =
    _salt `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` resourceSet

instance Prelude.NFData PutResourceSet where
  rnf PutResourceSet' {..} =
    Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf resourceSet

instance Core.ToHeaders PutResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.PutResourceSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutResourceSet where
  toJSON PutResourceSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagList" Core..=) Prelude.<$> tagList,
            Prelude.Just ("ResourceSet" Core..= resourceSet)
          ]
      )

instance Core.ToPath PutResourceSet where
  toPath = Prelude.const "/"

instance Core.ToQuery PutResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourceSetResponse' smart constructor.
data PutResourceSetResponse = PutResourceSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about the resource set.
    resourceSet :: ResourceSet,
    -- | The Amazon Resource Name (ARN) of the resource set.
    resourceSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putResourceSetResponse_httpStatus' - The response's http status code.
--
-- 'resourceSet', 'putResourceSetResponse_resourceSet' - Details about the resource set.
--
-- 'resourceSetArn', 'putResourceSetResponse_resourceSetArn' - The Amazon Resource Name (ARN) of the resource set.
newPutResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceSet'
  ResourceSet ->
  -- | 'resourceSetArn'
  Prelude.Text ->
  PutResourceSetResponse
newPutResourceSetResponse
  pHttpStatus_
  pResourceSet_
  pResourceSetArn_ =
    PutResourceSetResponse'
      { httpStatus = pHttpStatus_,
        resourceSet = pResourceSet_,
        resourceSetArn = pResourceSetArn_
      }

-- | The response's http status code.
putResourceSetResponse_httpStatus :: Lens.Lens' PutResourceSetResponse Prelude.Int
putResourceSetResponse_httpStatus = Lens.lens (\PutResourceSetResponse' {httpStatus} -> httpStatus) (\s@PutResourceSetResponse' {} a -> s {httpStatus = a} :: PutResourceSetResponse)

-- | Details about the resource set.
putResourceSetResponse_resourceSet :: Lens.Lens' PutResourceSetResponse ResourceSet
putResourceSetResponse_resourceSet = Lens.lens (\PutResourceSetResponse' {resourceSet} -> resourceSet) (\s@PutResourceSetResponse' {} a -> s {resourceSet = a} :: PutResourceSetResponse)

-- | The Amazon Resource Name (ARN) of the resource set.
putResourceSetResponse_resourceSetArn :: Lens.Lens' PutResourceSetResponse Prelude.Text
putResourceSetResponse_resourceSetArn = Lens.lens (\PutResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@PutResourceSetResponse' {} a -> s {resourceSetArn = a} :: PutResourceSetResponse)

instance Prelude.NFData PutResourceSetResponse where
  rnf PutResourceSetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceSet
      `Prelude.seq` Prelude.rnf resourceSetArn
