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
-- Module      : Amazonka.Connect.GetContactAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contact attributes for the specified contact.
module Amazonka.Connect.GetContactAttributes
  ( -- * Creating a Request
    GetContactAttributes (..),
    newGetContactAttributes,

    -- * Request Lenses
    getContactAttributes_instanceId,
    getContactAttributes_initialContactId,

    -- * Destructuring the Response
    GetContactAttributesResponse (..),
    newGetContactAttributesResponse,

    -- * Response Lenses
    getContactAttributesResponse_attributes,
    getContactAttributesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContactAttributes' smart constructor.
data GetContactAttributes = GetContactAttributes'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the initial contact.
    initialContactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getContactAttributes_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'initialContactId', 'getContactAttributes_initialContactId' - The identifier of the initial contact.
newGetContactAttributes ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'initialContactId'
  Prelude.Text ->
  GetContactAttributes
newGetContactAttributes
  pInstanceId_
  pInitialContactId_ =
    GetContactAttributes'
      { instanceId = pInstanceId_,
        initialContactId = pInitialContactId_
      }

-- | The identifier of the Amazon Connect instance.
getContactAttributes_instanceId :: Lens.Lens' GetContactAttributes Prelude.Text
getContactAttributes_instanceId = Lens.lens (\GetContactAttributes' {instanceId} -> instanceId) (\s@GetContactAttributes' {} a -> s {instanceId = a} :: GetContactAttributes)

-- | The identifier of the initial contact.
getContactAttributes_initialContactId :: Lens.Lens' GetContactAttributes Prelude.Text
getContactAttributes_initialContactId = Lens.lens (\GetContactAttributes' {initialContactId} -> initialContactId) (\s@GetContactAttributes' {} a -> s {initialContactId = a} :: GetContactAttributes)

instance Core.AWSRequest GetContactAttributes where
  type
    AWSResponse GetContactAttributes =
      GetContactAttributesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactAttributesResponse'
            Prelude.<$> (x Data..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContactAttributes where
  hashWithSalt _salt GetContactAttributes' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` initialContactId

instance Prelude.NFData GetContactAttributes where
  rnf GetContactAttributes' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf initialContactId

instance Data.ToHeaders GetContactAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetContactAttributes where
  toPath GetContactAttributes' {..} =
    Prelude.mconcat
      [ "/contact/attributes/",
        Data.toBS instanceId,
        "/",
        Data.toBS initialContactId
      ]

instance Data.ToQuery GetContactAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactAttributesResponse' smart constructor.
data GetContactAttributesResponse = GetContactAttributesResponse'
  { -- | Information about the attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getContactAttributesResponse_attributes' - Information about the attributes.
--
-- 'httpStatus', 'getContactAttributesResponse_httpStatus' - The response's http status code.
newGetContactAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactAttributesResponse
newGetContactAttributesResponse pHttpStatus_ =
  GetContactAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attributes.
getContactAttributesResponse_attributes :: Lens.Lens' GetContactAttributesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getContactAttributesResponse_attributes = Lens.lens (\GetContactAttributesResponse' {attributes} -> attributes) (\s@GetContactAttributesResponse' {} a -> s {attributes = a} :: GetContactAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getContactAttributesResponse_httpStatus :: Lens.Lens' GetContactAttributesResponse Prelude.Int
getContactAttributesResponse_httpStatus = Lens.lens (\GetContactAttributesResponse' {httpStatus} -> httpStatus) (\s@GetContactAttributesResponse' {} a -> s {httpStatus = a} :: GetContactAttributesResponse)

instance Prelude.NFData GetContactAttributesResponse where
  rnf GetContactAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
