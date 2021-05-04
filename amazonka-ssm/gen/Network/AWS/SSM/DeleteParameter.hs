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
-- Module      : Network.AWS.SSM.DeleteParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a parameter from the system.
module Network.AWS.SSM.DeleteParameter
  ( -- * Creating a Request
    DeleteParameter (..),
    newDeleteParameter,

    -- * Request Lenses
    deleteParameter_name,

    -- * Destructuring the Response
    DeleteParameterResponse (..),
    newDeleteParameterResponse,

    -- * Response Lenses
    deleteParameterResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteParameter' smart constructor.
data DeleteParameter = DeleteParameter'
  { -- | The name of the parameter to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteParameter_name' - The name of the parameter to delete.
newDeleteParameter ::
  -- | 'name'
  Prelude.Text ->
  DeleteParameter
newDeleteParameter pName_ =
  DeleteParameter' {name = pName_}

-- | The name of the parameter to delete.
deleteParameter_name :: Lens.Lens' DeleteParameter Prelude.Text
deleteParameter_name = Lens.lens (\DeleteParameter' {name} -> name) (\s@DeleteParameter' {} a -> s {name = a} :: DeleteParameter)

instance Prelude.AWSRequest DeleteParameter where
  type Rs DeleteParameter = DeleteParameterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteParameterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteParameter

instance Prelude.NFData DeleteParameter

instance Prelude.ToHeaders DeleteParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AmazonSSM.DeleteParameter" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteParameter where
  toJSON DeleteParameter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteParameter where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteParameterResponse' smart constructor.
data DeleteParameterResponse = DeleteParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteParameterResponse_httpStatus' - The response's http status code.
newDeleteParameterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteParameterResponse
newDeleteParameterResponse pHttpStatus_ =
  DeleteParameterResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteParameterResponse_httpStatus :: Lens.Lens' DeleteParameterResponse Prelude.Int
deleteParameterResponse_httpStatus = Lens.lens (\DeleteParameterResponse' {httpStatus} -> httpStatus) (\s@DeleteParameterResponse' {} a -> s {httpStatus = a} :: DeleteParameterResponse)

instance Prelude.NFData DeleteParameterResponse
