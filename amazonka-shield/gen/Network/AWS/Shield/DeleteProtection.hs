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
-- Module      : Network.AWS.Shield.DeleteProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Shield Advanced Protection.
module Network.AWS.Shield.DeleteProtection
  ( -- * Creating a Request
    DeleteProtection (..),
    newDeleteProtection,

    -- * Request Lenses
    deleteProtection_protectionId,

    -- * Destructuring the Response
    DeleteProtectionResponse (..),
    newDeleteProtectionResponse,

    -- * Response Lenses
    deleteProtectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDeleteProtection' smart constructor.
data DeleteProtection = DeleteProtection'
  { -- | The unique identifier (ID) for the Protection object to be deleted.
    protectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'deleteProtection_protectionId' - The unique identifier (ID) for the Protection object to be deleted.
newDeleteProtection ::
  -- | 'protectionId'
  Prelude.Text ->
  DeleteProtection
newDeleteProtection pProtectionId_ =
  DeleteProtection' {protectionId = pProtectionId_}

-- | The unique identifier (ID) for the Protection object to be deleted.
deleteProtection_protectionId :: Lens.Lens' DeleteProtection Prelude.Text
deleteProtection_protectionId = Lens.lens (\DeleteProtection' {protectionId} -> protectionId) (\s@DeleteProtection' {} a -> s {protectionId = a} :: DeleteProtection)

instance Prelude.AWSRequest DeleteProtection where
  type Rs DeleteProtection = DeleteProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProtection

instance Prelude.NFData DeleteProtection

instance Prelude.ToHeaders DeleteProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.DeleteProtection" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProtection where
  toJSON DeleteProtection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProtectionId" Prelude..= protectionId)
          ]
      )

instance Prelude.ToPath DeleteProtection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProtectionResponse' smart constructor.
data DeleteProtectionResponse = DeleteProtectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProtectionResponse_httpStatus' - The response's http status code.
newDeleteProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProtectionResponse
newDeleteProtectionResponse pHttpStatus_ =
  DeleteProtectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProtectionResponse_httpStatus :: Lens.Lens' DeleteProtectionResponse Prelude.Int
deleteProtectionResponse_httpStatus = Lens.lens (\DeleteProtectionResponse' {httpStatus} -> httpStatus) (\s@DeleteProtectionResponse' {} a -> s {httpStatus = a} :: DeleteProtectionResponse)

instance Prelude.NFData DeleteProtectionResponse
