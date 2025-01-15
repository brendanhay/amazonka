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
-- Module      : Amazonka.AlexaBusiness.GetContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contact details by the contact ARN.
module Amazonka.AlexaBusiness.GetContact
  ( -- * Creating a Request
    GetContact (..),
    newGetContact,

    -- * Request Lenses
    getContact_contactArn,

    -- * Destructuring the Response
    GetContactResponse (..),
    newGetContactResponse,

    -- * Response Lenses
    getContactResponse_contact,
    getContactResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContact' smart constructor.
data GetContact = GetContact'
  { -- | The ARN of the contact for which to request details.
    contactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'getContact_contactArn' - The ARN of the contact for which to request details.
newGetContact ::
  -- | 'contactArn'
  Prelude.Text ->
  GetContact
newGetContact pContactArn_ =
  GetContact' {contactArn = pContactArn_}

-- | The ARN of the contact for which to request details.
getContact_contactArn :: Lens.Lens' GetContact Prelude.Text
getContact_contactArn = Lens.lens (\GetContact' {contactArn} -> contactArn) (\s@GetContact' {} a -> s {contactArn = a} :: GetContact)

instance Core.AWSRequest GetContact where
  type AWSResponse GetContact = GetContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Prelude.<$> (x Data..?> "Contact")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContact where
  hashWithSalt _salt GetContact' {..} =
    _salt `Prelude.hashWithSalt` contactArn

instance Prelude.NFData GetContact where
  rnf GetContact' {..} = Prelude.rnf contactArn

instance Data.ToHeaders GetContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.GetContact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContact where
  toJSON GetContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContactArn" Data..= contactArn)]
      )

instance Data.ToPath GetContact where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | The details of the requested contact.
    contact :: Prelude.Maybe Contact,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contact', 'getContactResponse_contact' - The details of the requested contact.
--
-- 'httpStatus', 'getContactResponse_httpStatus' - The response's http status code.
newGetContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactResponse
newGetContactResponse pHttpStatus_ =
  GetContactResponse'
    { contact = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the requested contact.
getContactResponse_contact :: Lens.Lens' GetContactResponse (Prelude.Maybe Contact)
getContactResponse_contact = Lens.lens (\GetContactResponse' {contact} -> contact) (\s@GetContactResponse' {} a -> s {contact = a} :: GetContactResponse)

-- | The response's http status code.
getContactResponse_httpStatus :: Lens.Lens' GetContactResponse Prelude.Int
getContactResponse_httpStatus = Lens.lens (\GetContactResponse' {httpStatus} -> httpStatus) (\s@GetContactResponse' {} a -> s {httpStatus = a} :: GetContactResponse)

instance Prelude.NFData GetContactResponse where
  rnf GetContactResponse' {..} =
    Prelude.rnf contact `Prelude.seq`
      Prelude.rnf httpStatus
