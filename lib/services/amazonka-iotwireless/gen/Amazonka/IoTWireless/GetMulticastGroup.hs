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
-- Module      : Amazonka.IoTWireless.GetMulticastGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a multicast group.
module Amazonka.IoTWireless.GetMulticastGroup
  ( -- * Creating a Request
    GetMulticastGroup (..),
    newGetMulticastGroup,

    -- * Request Lenses
    getMulticastGroup_id,

    -- * Destructuring the Response
    GetMulticastGroupResponse (..),
    newGetMulticastGroupResponse,

    -- * Response Lenses
    getMulticastGroupResponse_arn,
    getMulticastGroupResponse_createdAt,
    getMulticastGroupResponse_description,
    getMulticastGroupResponse_id,
    getMulticastGroupResponse_loRaWAN,
    getMulticastGroupResponse_name,
    getMulticastGroupResponse_status,
    getMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMulticastGroup' smart constructor.
data GetMulticastGroup = GetMulticastGroup'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getMulticastGroup_id' - Undocumented member.
newGetMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  GetMulticastGroup
newGetMulticastGroup pId_ =
  GetMulticastGroup' {id = pId_}

-- | Undocumented member.
getMulticastGroup_id :: Lens.Lens' GetMulticastGroup Prelude.Text
getMulticastGroup_id = Lens.lens (\GetMulticastGroup' {id} -> id) (\s@GetMulticastGroup' {} a -> s {id = a} :: GetMulticastGroup)

instance Core.AWSRequest GetMulticastGroup where
  type
    AWSResponse GetMulticastGroup =
      GetMulticastGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMulticastGroupResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "LoRaWAN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMulticastGroup where
  hashWithSalt _salt GetMulticastGroup' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetMulticastGroup where
  rnf GetMulticastGroup' {..} = Prelude.rnf id

instance Data.ToHeaders GetMulticastGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetMulticastGroup where
  toPath GetMulticastGroup' {..} =
    Prelude.mconcat
      ["/multicast-groups/", Data.toBS id]

instance Data.ToQuery GetMulticastGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMulticastGroupResponse' smart constructor.
data GetMulticastGroupResponse = GetMulticastGroupResponse'
  { arn :: Prelude.Maybe Prelude.Text,
    createdAt :: Prelude.Maybe Data.POSIX,
    description :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text,
    loRaWAN :: Prelude.Maybe LoRaWANMulticastGet,
    name :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getMulticastGroupResponse_arn' - Undocumented member.
--
-- 'createdAt', 'getMulticastGroupResponse_createdAt' - Undocumented member.
--
-- 'description', 'getMulticastGroupResponse_description' - Undocumented member.
--
-- 'id', 'getMulticastGroupResponse_id' - Undocumented member.
--
-- 'loRaWAN', 'getMulticastGroupResponse_loRaWAN' - Undocumented member.
--
-- 'name', 'getMulticastGroupResponse_name' - Undocumented member.
--
-- 'status', 'getMulticastGroupResponse_status' - Undocumented member.
--
-- 'httpStatus', 'getMulticastGroupResponse_httpStatus' - The response's http status code.
newGetMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMulticastGroupResponse
newGetMulticastGroupResponse pHttpStatus_ =
  GetMulticastGroupResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getMulticastGroupResponse_arn :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe Prelude.Text)
getMulticastGroupResponse_arn = Lens.lens (\GetMulticastGroupResponse' {arn} -> arn) (\s@GetMulticastGroupResponse' {} a -> s {arn = a} :: GetMulticastGroupResponse)

-- | Undocumented member.
getMulticastGroupResponse_createdAt :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe Prelude.UTCTime)
getMulticastGroupResponse_createdAt = Lens.lens (\GetMulticastGroupResponse' {createdAt} -> createdAt) (\s@GetMulticastGroupResponse' {} a -> s {createdAt = a} :: GetMulticastGroupResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
getMulticastGroupResponse_description :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe Prelude.Text)
getMulticastGroupResponse_description = Lens.lens (\GetMulticastGroupResponse' {description} -> description) (\s@GetMulticastGroupResponse' {} a -> s {description = a} :: GetMulticastGroupResponse)

-- | Undocumented member.
getMulticastGroupResponse_id :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe Prelude.Text)
getMulticastGroupResponse_id = Lens.lens (\GetMulticastGroupResponse' {id} -> id) (\s@GetMulticastGroupResponse' {} a -> s {id = a} :: GetMulticastGroupResponse)

-- | Undocumented member.
getMulticastGroupResponse_loRaWAN :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe LoRaWANMulticastGet)
getMulticastGroupResponse_loRaWAN = Lens.lens (\GetMulticastGroupResponse' {loRaWAN} -> loRaWAN) (\s@GetMulticastGroupResponse' {} a -> s {loRaWAN = a} :: GetMulticastGroupResponse)

-- | Undocumented member.
getMulticastGroupResponse_name :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe Prelude.Text)
getMulticastGroupResponse_name = Lens.lens (\GetMulticastGroupResponse' {name} -> name) (\s@GetMulticastGroupResponse' {} a -> s {name = a} :: GetMulticastGroupResponse)

-- | Undocumented member.
getMulticastGroupResponse_status :: Lens.Lens' GetMulticastGroupResponse (Prelude.Maybe Prelude.Text)
getMulticastGroupResponse_status = Lens.lens (\GetMulticastGroupResponse' {status} -> status) (\s@GetMulticastGroupResponse' {} a -> s {status = a} :: GetMulticastGroupResponse)

-- | The response's http status code.
getMulticastGroupResponse_httpStatus :: Lens.Lens' GetMulticastGroupResponse Prelude.Int
getMulticastGroupResponse_httpStatus = Lens.lens (\GetMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@GetMulticastGroupResponse' {} a -> s {httpStatus = a} :: GetMulticastGroupResponse)

instance Prelude.NFData GetMulticastGroupResponse where
  rnf GetMulticastGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
