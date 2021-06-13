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
-- Module      : Network.AWS.Connect.GetContactAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contact attributes for the specified contact.
module Network.AWS.Connect.GetContactAttributes
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactAttributesResponse'
            Prelude.<$> (x Core..?> "Attributes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContactAttributes

instance Prelude.NFData GetContactAttributes

instance Core.ToHeaders GetContactAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetContactAttributes where
  toPath GetContactAttributes' {..} =
    Prelude.mconcat
      [ "/contact/attributes/",
        Core.toBS instanceId,
        "/",
        Core.toBS initialContactId
      ]

instance Core.ToQuery GetContactAttributes where
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
getContactAttributesResponse_attributes = Lens.lens (\GetContactAttributesResponse' {attributes} -> attributes) (\s@GetContactAttributesResponse' {} a -> s {attributes = a} :: GetContactAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContactAttributesResponse_httpStatus :: Lens.Lens' GetContactAttributesResponse Prelude.Int
getContactAttributesResponse_httpStatus = Lens.lens (\GetContactAttributesResponse' {httpStatus} -> httpStatus) (\s@GetContactAttributesResponse' {} a -> s {httpStatus = a} :: GetContactAttributesResponse)

instance Prelude.NFData GetContactAttributesResponse
