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
-- Module      : Network.AWS.Lightsail.GetContactMethods
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured contact methods. Specify a
-- protocol in your request to return information about a specific contact
-- method.
--
-- A contact method is used to send you notifications about your Amazon
-- Lightsail resources. You can add one email address and one mobile phone
-- number contact method in each AWS Region. However, SMS text messaging is
-- not supported in some AWS Regions, and SMS text messages cannot be sent
-- to some countries\/regions. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
module Network.AWS.Lightsail.GetContactMethods
  ( -- * Creating a Request
    GetContactMethods (..),
    newGetContactMethods,

    -- * Request Lenses
    getContactMethods_protocols,

    -- * Destructuring the Response
    GetContactMethodsResponse (..),
    newGetContactMethodsResponse,

    -- * Response Lenses
    getContactMethodsResponse_contactMethods,
    getContactMethodsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContactMethods' smart constructor.
data GetContactMethods = GetContactMethods'
  { -- | The protocols used to send notifications, such as @Email@, or @SMS@
    -- (text messaging).
    --
    -- Specify a protocol in your request to return information about a
    -- specific contact method protocol.
    protocols :: Prelude.Maybe [ContactProtocol]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactMethods' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocols', 'getContactMethods_protocols' - The protocols used to send notifications, such as @Email@, or @SMS@
-- (text messaging).
--
-- Specify a protocol in your request to return information about a
-- specific contact method protocol.
newGetContactMethods ::
  GetContactMethods
newGetContactMethods =
  GetContactMethods' {protocols = Prelude.Nothing}

-- | The protocols used to send notifications, such as @Email@, or @SMS@
-- (text messaging).
--
-- Specify a protocol in your request to return information about a
-- specific contact method protocol.
getContactMethods_protocols :: Lens.Lens' GetContactMethods (Prelude.Maybe [ContactProtocol])
getContactMethods_protocols = Lens.lens (\GetContactMethods' {protocols} -> protocols) (\s@GetContactMethods' {} a -> s {protocols = a} :: GetContactMethods) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest GetContactMethods where
  type
    AWSResponse GetContactMethods =
      GetContactMethodsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactMethodsResponse'
            Prelude.<$> (x Core..?> "contactMethods" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContactMethods

instance Prelude.NFData GetContactMethods

instance Core.ToHeaders GetContactMethods where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContactMethods" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetContactMethods where
  toJSON GetContactMethods' {..} =
    Core.object
      ( Prelude.catMaybes
          [("protocols" Core..=) Prelude.<$> protocols]
      )

instance Core.ToPath GetContactMethods where
  toPath = Prelude.const "/"

instance Core.ToQuery GetContactMethods where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactMethodsResponse' smart constructor.
data GetContactMethodsResponse = GetContactMethodsResponse'
  { -- | An array of objects that describe the contact methods.
    contactMethods :: Prelude.Maybe [ContactMethod],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactMethodsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactMethods', 'getContactMethodsResponse_contactMethods' - An array of objects that describe the contact methods.
--
-- 'httpStatus', 'getContactMethodsResponse_httpStatus' - The response's http status code.
newGetContactMethodsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactMethodsResponse
newGetContactMethodsResponse pHttpStatus_ =
  GetContactMethodsResponse'
    { contactMethods =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the contact methods.
getContactMethodsResponse_contactMethods :: Lens.Lens' GetContactMethodsResponse (Prelude.Maybe [ContactMethod])
getContactMethodsResponse_contactMethods = Lens.lens (\GetContactMethodsResponse' {contactMethods} -> contactMethods) (\s@GetContactMethodsResponse' {} a -> s {contactMethods = a} :: GetContactMethodsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContactMethodsResponse_httpStatus :: Lens.Lens' GetContactMethodsResponse Prelude.Int
getContactMethodsResponse_httpStatus = Lens.lens (\GetContactMethodsResponse' {httpStatus} -> httpStatus) (\s@GetContactMethodsResponse' {} a -> s {httpStatus = a} :: GetContactMethodsResponse)

instance Prelude.NFData GetContactMethodsResponse
