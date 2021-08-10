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
-- Module      : Network.AWS.Lightsail.DeleteContactMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a contact method.
--
-- A contact method is used to send you notifications about your Amazon
-- Lightsail resources. You can add one email address and one mobile phone
-- number contact method in each AWS Region. However, SMS text messaging is
-- not supported in some AWS Regions, and SMS text messages cannot be sent
-- to some countries\/regions. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
module Network.AWS.Lightsail.DeleteContactMethod
  ( -- * Creating a Request
    DeleteContactMethod (..),
    newDeleteContactMethod,

    -- * Request Lenses
    deleteContactMethod_protocol,

    -- * Destructuring the Response
    DeleteContactMethodResponse (..),
    newDeleteContactMethodResponse,

    -- * Response Lenses
    deleteContactMethodResponse_operations,
    deleteContactMethodResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteContactMethod' smart constructor.
data DeleteContactMethod = DeleteContactMethod'
  { -- | The protocol that will be deleted, such as @Email@ or @SMS@ (text
    -- messaging).
    --
    -- To delete an @Email@ and an @SMS@ contact method if you added both, you
    -- must run separate @DeleteContactMethod@ actions to delete each protocol.
    protocol :: ContactProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'deleteContactMethod_protocol' - The protocol that will be deleted, such as @Email@ or @SMS@ (text
-- messaging).
--
-- To delete an @Email@ and an @SMS@ contact method if you added both, you
-- must run separate @DeleteContactMethod@ actions to delete each protocol.
newDeleteContactMethod ::
  -- | 'protocol'
  ContactProtocol ->
  DeleteContactMethod
newDeleteContactMethod pProtocol_ =
  DeleteContactMethod' {protocol = pProtocol_}

-- | The protocol that will be deleted, such as @Email@ or @SMS@ (text
-- messaging).
--
-- To delete an @Email@ and an @SMS@ contact method if you added both, you
-- must run separate @DeleteContactMethod@ actions to delete each protocol.
deleteContactMethod_protocol :: Lens.Lens' DeleteContactMethod ContactProtocol
deleteContactMethod_protocol = Lens.lens (\DeleteContactMethod' {protocol} -> protocol) (\s@DeleteContactMethod' {} a -> s {protocol = a} :: DeleteContactMethod)

instance Core.AWSRequest DeleteContactMethod where
  type
    AWSResponse DeleteContactMethod =
      DeleteContactMethodResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteContactMethodResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContactMethod

instance Prelude.NFData DeleteContactMethod

instance Core.ToHeaders DeleteContactMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteContactMethod" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteContactMethod where
  toJSON DeleteContactMethod' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("protocol" Core..= protocol)]
      )

instance Core.ToPath DeleteContactMethod where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteContactMethod where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactMethodResponse' smart constructor.
data DeleteContactMethodResponse = DeleteContactMethodResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteContactMethodResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteContactMethodResponse_httpStatus' - The response's http status code.
newDeleteContactMethodResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContactMethodResponse
newDeleteContactMethodResponse pHttpStatus_ =
  DeleteContactMethodResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteContactMethodResponse_operations :: Lens.Lens' DeleteContactMethodResponse (Prelude.Maybe [Operation])
deleteContactMethodResponse_operations = Lens.lens (\DeleteContactMethodResponse' {operations} -> operations) (\s@DeleteContactMethodResponse' {} a -> s {operations = a} :: DeleteContactMethodResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteContactMethodResponse_httpStatus :: Lens.Lens' DeleteContactMethodResponse Prelude.Int
deleteContactMethodResponse_httpStatus = Lens.lens (\DeleteContactMethodResponse' {httpStatus} -> httpStatus) (\s@DeleteContactMethodResponse' {} a -> s {httpStatus = a} :: DeleteContactMethodResponse)

instance Prelude.NFData DeleteContactMethodResponse
