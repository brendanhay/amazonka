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
-- Module      : Network.AWS.AlexaBusiness.GetContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contact details by the contact ARN.
module Network.AWS.AlexaBusiness.GetContact
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContact' smart constructor.
data GetContact = GetContact'
  { -- | The ARN of the contact for which to request details.
    contactArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetContact
newGetContact pContactArn_ =
  GetContact' {contactArn = pContactArn_}

-- | The ARN of the contact for which to request details.
getContact_contactArn :: Lens.Lens' GetContact Core.Text
getContact_contactArn = Lens.lens (\GetContact' {contactArn} -> contactArn) (\s@GetContact' {} a -> s {contactArn = a} :: GetContact)

instance Core.AWSRequest GetContact where
  type AWSResponse GetContact = GetContactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Core.<$> (x Core..?> "Contact")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetContact

instance Core.NFData GetContact

instance Core.ToHeaders GetContact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.GetContact" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetContact where
  toJSON GetContact' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContactArn" Core..= contactArn)]
      )

instance Core.ToPath GetContact where
  toPath = Core.const "/"

instance Core.ToQuery GetContact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | The details of the requested contact.
    contact :: Core.Maybe Contact,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  GetContactResponse
newGetContactResponse pHttpStatus_ =
  GetContactResponse'
    { contact = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the requested contact.
getContactResponse_contact :: Lens.Lens' GetContactResponse (Core.Maybe Contact)
getContactResponse_contact = Lens.lens (\GetContactResponse' {contact} -> contact) (\s@GetContactResponse' {} a -> s {contact = a} :: GetContactResponse)

-- | The response's http status code.
getContactResponse_httpStatus :: Lens.Lens' GetContactResponse Core.Int
getContactResponse_httpStatus = Lens.lens (\GetContactResponse' {httpStatus} -> httpStatus) (\s@GetContactResponse' {} a -> s {httpStatus = a} :: GetContactResponse)

instance Core.NFData GetContactResponse
