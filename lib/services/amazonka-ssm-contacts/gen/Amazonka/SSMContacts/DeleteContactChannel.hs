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
-- Module      : Amazonka.SSMContacts.DeleteContactChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To no longer receive engagements on a contact channel, you can delete
-- the channel from a contact. Deleting the contact channel removes it from
-- the contact\'s engagement plan. If you delete the only contact channel
-- for a contact, you won\'t be able to engage that contact during an
-- incident.
module Amazonka.SSMContacts.DeleteContactChannel
  ( -- * Creating a Request
    DeleteContactChannel (..),
    newDeleteContactChannel,

    -- * Request Lenses
    deleteContactChannel_contactChannelId,

    -- * Destructuring the Response
    DeleteContactChannelResponse (..),
    newDeleteContactChannelResponse,

    -- * Response Lenses
    deleteContactChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newDeleteContactChannel' smart constructor.
data DeleteContactChannel = DeleteContactChannel'
  { -- | The Amazon Resource Name (ARN) of the contact channel.
    contactChannelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactChannelId', 'deleteContactChannel_contactChannelId' - The Amazon Resource Name (ARN) of the contact channel.
newDeleteContactChannel ::
  -- | 'contactChannelId'
  Prelude.Text ->
  DeleteContactChannel
newDeleteContactChannel pContactChannelId_ =
  DeleteContactChannel'
    { contactChannelId =
        pContactChannelId_
    }

-- | The Amazon Resource Name (ARN) of the contact channel.
deleteContactChannel_contactChannelId :: Lens.Lens' DeleteContactChannel Prelude.Text
deleteContactChannel_contactChannelId = Lens.lens (\DeleteContactChannel' {contactChannelId} -> contactChannelId) (\s@DeleteContactChannel' {} a -> s {contactChannelId = a} :: DeleteContactChannel)

instance Core.AWSRequest DeleteContactChannel where
  type
    AWSResponse DeleteContactChannel =
      DeleteContactChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContactChannel where
  hashWithSalt _salt DeleteContactChannel' {..} =
    _salt `Prelude.hashWithSalt` contactChannelId

instance Prelude.NFData DeleteContactChannel where
  rnf DeleteContactChannel' {..} =
    Prelude.rnf contactChannelId

instance Data.ToHeaders DeleteContactChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.DeleteContactChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteContactChannel where
  toJSON DeleteContactChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContactChannelId" Data..= contactChannelId)
          ]
      )

instance Data.ToPath DeleteContactChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteContactChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactChannelResponse' smart constructor.
data DeleteContactChannelResponse = DeleteContactChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContactChannelResponse_httpStatus' - The response's http status code.
newDeleteContactChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContactChannelResponse
newDeleteContactChannelResponse pHttpStatus_ =
  DeleteContactChannelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContactChannelResponse_httpStatus :: Lens.Lens' DeleteContactChannelResponse Prelude.Int
deleteContactChannelResponse_httpStatus = Lens.lens (\DeleteContactChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteContactChannelResponse' {} a -> s {httpStatus = a} :: DeleteContactChannelResponse)

instance Prelude.NFData DeleteContactChannelResponse where
  rnf DeleteContactChannelResponse' {..} =
    Prelude.rnf httpStatus
