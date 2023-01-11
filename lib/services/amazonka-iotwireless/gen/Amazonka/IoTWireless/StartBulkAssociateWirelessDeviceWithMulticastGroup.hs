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
-- Module      : Amazonka.IoTWireless.StartBulkAssociateWirelessDeviceWithMulticastGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a bulk association of all qualifying wireless devices with a
-- multicast group.
module Amazonka.IoTWireless.StartBulkAssociateWirelessDeviceWithMulticastGroup
  ( -- * Creating a Request
    StartBulkAssociateWirelessDeviceWithMulticastGroup (..),
    newStartBulkAssociateWirelessDeviceWithMulticastGroup,

    -- * Request Lenses
    startBulkAssociateWirelessDeviceWithMulticastGroup_queryString,
    startBulkAssociateWirelessDeviceWithMulticastGroup_tags,
    startBulkAssociateWirelessDeviceWithMulticastGroup_id,

    -- * Destructuring the Response
    StartBulkAssociateWirelessDeviceWithMulticastGroupResponse (..),
    newStartBulkAssociateWirelessDeviceWithMulticastGroupResponse,

    -- * Response Lenses
    startBulkAssociateWirelessDeviceWithMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartBulkAssociateWirelessDeviceWithMulticastGroup' smart constructor.
data StartBulkAssociateWirelessDeviceWithMulticastGroup = StartBulkAssociateWirelessDeviceWithMulticastGroup'
  { queryString :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBulkAssociateWirelessDeviceWithMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'startBulkAssociateWirelessDeviceWithMulticastGroup_queryString' - Undocumented member.
--
-- 'tags', 'startBulkAssociateWirelessDeviceWithMulticastGroup_tags' - Undocumented member.
--
-- 'id', 'startBulkAssociateWirelessDeviceWithMulticastGroup_id' - Undocumented member.
newStartBulkAssociateWirelessDeviceWithMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  StartBulkAssociateWirelessDeviceWithMulticastGroup
newStartBulkAssociateWirelessDeviceWithMulticastGroup
  pId_ =
    StartBulkAssociateWirelessDeviceWithMulticastGroup'
      { queryString =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        id = pId_
      }

-- | Undocumented member.
startBulkAssociateWirelessDeviceWithMulticastGroup_queryString :: Lens.Lens' StartBulkAssociateWirelessDeviceWithMulticastGroup (Prelude.Maybe Prelude.Text)
startBulkAssociateWirelessDeviceWithMulticastGroup_queryString = Lens.lens (\StartBulkAssociateWirelessDeviceWithMulticastGroup' {queryString} -> queryString) (\s@StartBulkAssociateWirelessDeviceWithMulticastGroup' {} a -> s {queryString = a} :: StartBulkAssociateWirelessDeviceWithMulticastGroup)

-- | Undocumented member.
startBulkAssociateWirelessDeviceWithMulticastGroup_tags :: Lens.Lens' StartBulkAssociateWirelessDeviceWithMulticastGroup (Prelude.Maybe [Tag])
startBulkAssociateWirelessDeviceWithMulticastGroup_tags = Lens.lens (\StartBulkAssociateWirelessDeviceWithMulticastGroup' {tags} -> tags) (\s@StartBulkAssociateWirelessDeviceWithMulticastGroup' {} a -> s {tags = a} :: StartBulkAssociateWirelessDeviceWithMulticastGroup) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
startBulkAssociateWirelessDeviceWithMulticastGroup_id :: Lens.Lens' StartBulkAssociateWirelessDeviceWithMulticastGroup Prelude.Text
startBulkAssociateWirelessDeviceWithMulticastGroup_id = Lens.lens (\StartBulkAssociateWirelessDeviceWithMulticastGroup' {id} -> id) (\s@StartBulkAssociateWirelessDeviceWithMulticastGroup' {} a -> s {id = a} :: StartBulkAssociateWirelessDeviceWithMulticastGroup)

instance
  Core.AWSRequest
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  type
    AWSResponse
      StartBulkAssociateWirelessDeviceWithMulticastGroup =
      StartBulkAssociateWirelessDeviceWithMulticastGroupResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartBulkAssociateWirelessDeviceWithMulticastGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  hashWithSalt
    _salt
    StartBulkAssociateWirelessDeviceWithMulticastGroup' {..} =
      _salt `Prelude.hashWithSalt` queryString
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  rnf
    StartBulkAssociateWirelessDeviceWithMulticastGroup' {..} =
      Prelude.rnf queryString
        `Prelude.seq` Prelude.rnf tags
        `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  toJSON
    StartBulkAssociateWirelessDeviceWithMulticastGroup' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("QueryString" Data..=) Prelude.<$> queryString,
              ("Tags" Data..=) Prelude.<$> tags
            ]
        )

instance
  Data.ToPath
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  toPath
    StartBulkAssociateWirelessDeviceWithMulticastGroup' {..} =
      Prelude.mconcat
        ["/multicast-groups/", Data.toBS id, "/bulk"]

instance
  Data.ToQuery
    StartBulkAssociateWirelessDeviceWithMulticastGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBulkAssociateWirelessDeviceWithMulticastGroupResponse' smart constructor.
data StartBulkAssociateWirelessDeviceWithMulticastGroupResponse = StartBulkAssociateWirelessDeviceWithMulticastGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBulkAssociateWirelessDeviceWithMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startBulkAssociateWirelessDeviceWithMulticastGroupResponse_httpStatus' - The response's http status code.
newStartBulkAssociateWirelessDeviceWithMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartBulkAssociateWirelessDeviceWithMulticastGroupResponse
newStartBulkAssociateWirelessDeviceWithMulticastGroupResponse
  pHttpStatus_ =
    StartBulkAssociateWirelessDeviceWithMulticastGroupResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
startBulkAssociateWirelessDeviceWithMulticastGroupResponse_httpStatus :: Lens.Lens' StartBulkAssociateWirelessDeviceWithMulticastGroupResponse Prelude.Int
startBulkAssociateWirelessDeviceWithMulticastGroupResponse_httpStatus = Lens.lens (\StartBulkAssociateWirelessDeviceWithMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@StartBulkAssociateWirelessDeviceWithMulticastGroupResponse' {} a -> s {httpStatus = a} :: StartBulkAssociateWirelessDeviceWithMulticastGroupResponse)

instance
  Prelude.NFData
    StartBulkAssociateWirelessDeviceWithMulticastGroupResponse
  where
  rnf
    StartBulkAssociateWirelessDeviceWithMulticastGroupResponse' {..} =
      Prelude.rnf httpStatus
