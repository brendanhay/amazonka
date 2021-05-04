{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.DeleteClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a classifier from the Data Catalog.
module Network.AWS.Glue.DeleteClassifier
  ( -- * Creating a Request
    DeleteClassifier (..),
    newDeleteClassifier,

    -- * Request Lenses
    deleteClassifier_name,

    -- * Destructuring the Response
    DeleteClassifierResponse (..),
    newDeleteClassifierResponse,

    -- * Response Lenses
    deleteClassifierResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteClassifier' smart constructor.
data DeleteClassifier = DeleteClassifier'
  { -- | Name of the classifier to remove.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteClassifier_name' - Name of the classifier to remove.
newDeleteClassifier ::
  -- | 'name'
  Prelude.Text ->
  DeleteClassifier
newDeleteClassifier pName_ =
  DeleteClassifier' {name = pName_}

-- | Name of the classifier to remove.
deleteClassifier_name :: Lens.Lens' DeleteClassifier Prelude.Text
deleteClassifier_name = Lens.lens (\DeleteClassifier' {name} -> name) (\s@DeleteClassifier' {} a -> s {name = a} :: DeleteClassifier)

instance Prelude.AWSRequest DeleteClassifier where
  type Rs DeleteClassifier = DeleteClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteClassifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClassifier

instance Prelude.NFData DeleteClassifier

instance Prelude.ToHeaders DeleteClassifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeleteClassifier" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteClassifier where
  toJSON DeleteClassifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteClassifier where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteClassifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClassifierResponse' smart constructor.
data DeleteClassifierResponse = DeleteClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteClassifierResponse_httpStatus' - The response's http status code.
newDeleteClassifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClassifierResponse
newDeleteClassifierResponse pHttpStatus_ =
  DeleteClassifierResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteClassifierResponse_httpStatus :: Lens.Lens' DeleteClassifierResponse Prelude.Int
deleteClassifierResponse_httpStatus = Lens.lens (\DeleteClassifierResponse' {httpStatus} -> httpStatus) (\s@DeleteClassifierResponse' {} a -> s {httpStatus = a} :: DeleteClassifierResponse)

instance Prelude.NFData DeleteClassifierResponse
