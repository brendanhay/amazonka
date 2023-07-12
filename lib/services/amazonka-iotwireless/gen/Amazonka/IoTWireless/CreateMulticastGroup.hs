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
-- Module      : Amazonka.IoTWireless.CreateMulticastGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multicast group.
module Amazonka.IoTWireless.CreateMulticastGroup
  ( -- * Creating a Request
    CreateMulticastGroup (..),
    newCreateMulticastGroup,

    -- * Request Lenses
    createMulticastGroup_clientRequestToken,
    createMulticastGroup_description,
    createMulticastGroup_name,
    createMulticastGroup_tags,
    createMulticastGroup_loRaWAN,

    -- * Destructuring the Response
    CreateMulticastGroupResponse (..),
    newCreateMulticastGroupResponse,

    -- * Response Lenses
    createMulticastGroupResponse_arn,
    createMulticastGroupResponse_id,
    createMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMulticastGroup' smart constructor.
data CreateMulticastGroup = CreateMulticastGroup'
  { -- | Each resource must have a unique client request token. If you try to
    -- create a new resource with the same token as a resource that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the multicast group.
    description :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    loRaWAN :: LoRaWANMulticast
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createMulticastGroup_clientRequestToken' - Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
--
-- 'description', 'createMulticastGroup_description' - The description of the multicast group.
--
-- 'name', 'createMulticastGroup_name' - Undocumented member.
--
-- 'tags', 'createMulticastGroup_tags' - Undocumented member.
--
-- 'loRaWAN', 'createMulticastGroup_loRaWAN' - Undocumented member.
newCreateMulticastGroup ::
  -- | 'loRaWAN'
  LoRaWANMulticast ->
  CreateMulticastGroup
newCreateMulticastGroup pLoRaWAN_ =
  CreateMulticastGroup'
    { clientRequestToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      loRaWAN = pLoRaWAN_
    }

-- | Each resource must have a unique client request token. If you try to
-- create a new resource with the same token as a resource that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
createMulticastGroup_clientRequestToken :: Lens.Lens' CreateMulticastGroup (Prelude.Maybe Prelude.Text)
createMulticastGroup_clientRequestToken = Lens.lens (\CreateMulticastGroup' {clientRequestToken} -> clientRequestToken) (\s@CreateMulticastGroup' {} a -> s {clientRequestToken = a} :: CreateMulticastGroup)

-- | The description of the multicast group.
createMulticastGroup_description :: Lens.Lens' CreateMulticastGroup (Prelude.Maybe Prelude.Text)
createMulticastGroup_description = Lens.lens (\CreateMulticastGroup' {description} -> description) (\s@CreateMulticastGroup' {} a -> s {description = a} :: CreateMulticastGroup)

-- | Undocumented member.
createMulticastGroup_name :: Lens.Lens' CreateMulticastGroup (Prelude.Maybe Prelude.Text)
createMulticastGroup_name = Lens.lens (\CreateMulticastGroup' {name} -> name) (\s@CreateMulticastGroup' {} a -> s {name = a} :: CreateMulticastGroup)

-- | Undocumented member.
createMulticastGroup_tags :: Lens.Lens' CreateMulticastGroup (Prelude.Maybe [Tag])
createMulticastGroup_tags = Lens.lens (\CreateMulticastGroup' {tags} -> tags) (\s@CreateMulticastGroup' {} a -> s {tags = a} :: CreateMulticastGroup) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createMulticastGroup_loRaWAN :: Lens.Lens' CreateMulticastGroup LoRaWANMulticast
createMulticastGroup_loRaWAN = Lens.lens (\CreateMulticastGroup' {loRaWAN} -> loRaWAN) (\s@CreateMulticastGroup' {} a -> s {loRaWAN = a} :: CreateMulticastGroup)

instance Core.AWSRequest CreateMulticastGroup where
  type
    AWSResponse CreateMulticastGroup =
      CreateMulticastGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMulticastGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMulticastGroup where
  hashWithSalt _salt CreateMulticastGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` loRaWAN

instance Prelude.NFData CreateMulticastGroup where
  rnf CreateMulticastGroup' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf loRaWAN

instance Data.ToHeaders CreateMulticastGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMulticastGroup where
  toJSON CreateMulticastGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("LoRaWAN" Data..= loRaWAN)
          ]
      )

instance Data.ToPath CreateMulticastGroup where
  toPath = Prelude.const "/multicast-groups"

instance Data.ToQuery CreateMulticastGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMulticastGroupResponse' smart constructor.
data CreateMulticastGroupResponse = CreateMulticastGroupResponse'
  { arn :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createMulticastGroupResponse_arn' - Undocumented member.
--
-- 'id', 'createMulticastGroupResponse_id' - Undocumented member.
--
-- 'httpStatus', 'createMulticastGroupResponse_httpStatus' - The response's http status code.
newCreateMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMulticastGroupResponse
newCreateMulticastGroupResponse pHttpStatus_ =
  CreateMulticastGroupResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createMulticastGroupResponse_arn :: Lens.Lens' CreateMulticastGroupResponse (Prelude.Maybe Prelude.Text)
createMulticastGroupResponse_arn = Lens.lens (\CreateMulticastGroupResponse' {arn} -> arn) (\s@CreateMulticastGroupResponse' {} a -> s {arn = a} :: CreateMulticastGroupResponse)

-- | Undocumented member.
createMulticastGroupResponse_id :: Lens.Lens' CreateMulticastGroupResponse (Prelude.Maybe Prelude.Text)
createMulticastGroupResponse_id = Lens.lens (\CreateMulticastGroupResponse' {id} -> id) (\s@CreateMulticastGroupResponse' {} a -> s {id = a} :: CreateMulticastGroupResponse)

-- | The response's http status code.
createMulticastGroupResponse_httpStatus :: Lens.Lens' CreateMulticastGroupResponse Prelude.Int
createMulticastGroupResponse_httpStatus = Lens.lens (\CreateMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@CreateMulticastGroupResponse' {} a -> s {httpStatus = a} :: CreateMulticastGroupResponse)

instance Prelude.NFData CreateMulticastGroupResponse where
  rnf CreateMulticastGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
