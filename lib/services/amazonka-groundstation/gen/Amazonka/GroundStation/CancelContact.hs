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
-- Module      : Amazonka.GroundStation.CancelContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a contact with a specified contact ID.
module Amazonka.GroundStation.CancelContact
  ( -- * Creating a Request
    CancelContact (..),
    newCancelContact,

    -- * Request Lenses
    cancelContact_contactId,

    -- * Destructuring the Response
    ContactIdResponse (..),
    newContactIdResponse,

    -- * Response Lenses
    contactIdResponse_contactId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCancelContact' smart constructor.
data CancelContact = CancelContact'
  { -- | UUID of a contact.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'cancelContact_contactId' - UUID of a contact.
newCancelContact ::
  -- | 'contactId'
  Prelude.Text ->
  CancelContact
newCancelContact pContactId_ =
  CancelContact' {contactId = pContactId_}

-- | UUID of a contact.
cancelContact_contactId :: Lens.Lens' CancelContact Prelude.Text
cancelContact_contactId = Lens.lens (\CancelContact' {contactId} -> contactId) (\s@CancelContact' {} a -> s {contactId = a} :: CancelContact)

instance Core.AWSRequest CancelContact where
  type AWSResponse CancelContact = ContactIdResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CancelContact where
  hashWithSalt _salt CancelContact' {..} =
    _salt `Prelude.hashWithSalt` contactId

instance Prelude.NFData CancelContact where
  rnf CancelContact' {..} = Prelude.rnf contactId

instance Core.ToHeaders CancelContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath CancelContact where
  toPath CancelContact' {..} =
    Prelude.mconcat ["/contact/", Core.toBS contactId]

instance Core.ToQuery CancelContact where
  toQuery = Prelude.const Prelude.mempty
