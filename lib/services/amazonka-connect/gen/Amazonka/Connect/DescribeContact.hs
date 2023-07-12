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
-- Module      : Amazonka.Connect.DescribeContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Describes the specified contact.
--
-- Contact information remains available in Amazon Connect for 24 months,
-- and then it is deleted.
--
-- Only data from November 12, 2021, and later is returned by this API.
module Amazonka.Connect.DescribeContact
  ( -- * Creating a Request
    DescribeContact (..),
    newDescribeContact,

    -- * Request Lenses
    describeContact_instanceId,
    describeContact_contactId,

    -- * Destructuring the Response
    DescribeContactResponse (..),
    newDescribeContactResponse,

    -- * Response Lenses
    describeContactResponse_contact,
    describeContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContact' smart constructor.
data DescribeContact = DescribeContact'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'describeContact_contactId' - The identifier of the contact.
newDescribeContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  DescribeContact
newDescribeContact pInstanceId_ pContactId_ =
  DescribeContact'
    { instanceId = pInstanceId_,
      contactId = pContactId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeContact_instanceId :: Lens.Lens' DescribeContact Prelude.Text
describeContact_instanceId = Lens.lens (\DescribeContact' {instanceId} -> instanceId) (\s@DescribeContact' {} a -> s {instanceId = a} :: DescribeContact)

-- | The identifier of the contact.
describeContact_contactId :: Lens.Lens' DescribeContact Prelude.Text
describeContact_contactId = Lens.lens (\DescribeContact' {contactId} -> contactId) (\s@DescribeContact' {} a -> s {contactId = a} :: DescribeContact)

instance Core.AWSRequest DescribeContact where
  type
    AWSResponse DescribeContact =
      DescribeContactResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContactResponse'
            Prelude.<$> (x Data..?> "Contact")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContact where
  hashWithSalt _salt DescribeContact' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData DescribeContact where
  rnf DescribeContact' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId

instance Data.ToHeaders DescribeContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeContact where
  toPath DescribeContact' {..} =
    Prelude.mconcat
      [ "/contacts/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactId
      ]

instance Data.ToQuery DescribeContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContactResponse' smart constructor.
data DescribeContactResponse = DescribeContactResponse'
  { -- | Information about the contact.
    contact :: Prelude.Maybe Contact,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contact', 'describeContactResponse_contact' - Information about the contact.
--
-- 'httpStatus', 'describeContactResponse_httpStatus' - The response's http status code.
newDescribeContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContactResponse
newDescribeContactResponse pHttpStatus_ =
  DescribeContactResponse'
    { contact = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the contact.
describeContactResponse_contact :: Lens.Lens' DescribeContactResponse (Prelude.Maybe Contact)
describeContactResponse_contact = Lens.lens (\DescribeContactResponse' {contact} -> contact) (\s@DescribeContactResponse' {} a -> s {contact = a} :: DescribeContactResponse)

-- | The response's http status code.
describeContactResponse_httpStatus :: Lens.Lens' DescribeContactResponse Prelude.Int
describeContactResponse_httpStatus = Lens.lens (\DescribeContactResponse' {httpStatus} -> httpStatus) (\s@DescribeContactResponse' {} a -> s {httpStatus = a} :: DescribeContactResponse)

instance Prelude.NFData DescribeContactResponse where
  rnf DescribeContactResponse' {..} =
    Prelude.rnf contact
      `Prelude.seq` Prelude.rnf httpStatus
