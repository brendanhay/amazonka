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
-- Module      : Amazonka.IoT.RemoveThingFromThingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the specified thing from the specified group.
--
-- You must specify either a @thingGroupArn@ or a @thingGroupName@ to
-- identify the thing group and either a @thingArn@ or a @thingName@ to
-- identify the thing to remove from the thing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RemoveThingFromThingGroup>
-- action.
module Amazonka.IoT.RemoveThingFromThingGroup
  ( -- * Creating a Request
    RemoveThingFromThingGroup (..),
    newRemoveThingFromThingGroup,

    -- * Request Lenses
    removeThingFromThingGroup_thingArn,
    removeThingFromThingGroup_thingGroupArn,
    removeThingFromThingGroup_thingGroupName,
    removeThingFromThingGroup_thingName,

    -- * Destructuring the Response
    RemoveThingFromThingGroupResponse (..),
    newRemoveThingFromThingGroupResponse,

    -- * Response Lenses
    removeThingFromThingGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveThingFromThingGroup' smart constructor.
data RemoveThingFromThingGroup = RemoveThingFromThingGroup'
  { -- | The ARN of the thing to remove from the group.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The group ARN.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The group name.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to remove from the group.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveThingFromThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingArn', 'removeThingFromThingGroup_thingArn' - The ARN of the thing to remove from the group.
--
-- 'thingGroupArn', 'removeThingFromThingGroup_thingGroupArn' - The group ARN.
--
-- 'thingGroupName', 'removeThingFromThingGroup_thingGroupName' - The group name.
--
-- 'thingName', 'removeThingFromThingGroup_thingName' - The name of the thing to remove from the group.
newRemoveThingFromThingGroup ::
  RemoveThingFromThingGroup
newRemoveThingFromThingGroup =
  RemoveThingFromThingGroup'
    { thingArn =
        Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | The ARN of the thing to remove from the group.
removeThingFromThingGroup_thingArn :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingArn = Lens.lens (\RemoveThingFromThingGroup' {thingArn} -> thingArn) (\s@RemoveThingFromThingGroup' {} a -> s {thingArn = a} :: RemoveThingFromThingGroup)

-- | The group ARN.
removeThingFromThingGroup_thingGroupArn :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingGroupArn = Lens.lens (\RemoveThingFromThingGroup' {thingGroupArn} -> thingGroupArn) (\s@RemoveThingFromThingGroup' {} a -> s {thingGroupArn = a} :: RemoveThingFromThingGroup)

-- | The group name.
removeThingFromThingGroup_thingGroupName :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingGroupName = Lens.lens (\RemoveThingFromThingGroup' {thingGroupName} -> thingGroupName) (\s@RemoveThingFromThingGroup' {} a -> s {thingGroupName = a} :: RemoveThingFromThingGroup)

-- | The name of the thing to remove from the group.
removeThingFromThingGroup_thingName :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingName = Lens.lens (\RemoveThingFromThingGroup' {thingName} -> thingName) (\s@RemoveThingFromThingGroup' {} a -> s {thingName = a} :: RemoveThingFromThingGroup)

instance Core.AWSRequest RemoveThingFromThingGroup where
  type
    AWSResponse RemoveThingFromThingGroup =
      RemoveThingFromThingGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveThingFromThingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveThingFromThingGroup where
  hashWithSalt _salt RemoveThingFromThingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` thingGroupArn
      `Prelude.hashWithSalt` thingGroupName
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData RemoveThingFromThingGroup where
  rnf RemoveThingFromThingGroup' {..} =
    Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf thingGroupArn
      `Prelude.seq` Prelude.rnf thingGroupName
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders RemoveThingFromThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RemoveThingFromThingGroup where
  toJSON RemoveThingFromThingGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("thingArn" Data..=) Prelude.<$> thingArn,
            ("thingGroupArn" Data..=) Prelude.<$> thingGroupArn,
            ("thingGroupName" Data..=)
              Prelude.<$> thingGroupName,
            ("thingName" Data..=) Prelude.<$> thingName
          ]
      )

instance Data.ToPath RemoveThingFromThingGroup where
  toPath =
    Prelude.const
      "/thing-groups/removeThingFromThingGroup"

instance Data.ToQuery RemoveThingFromThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveThingFromThingGroupResponse' smart constructor.
data RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveThingFromThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeThingFromThingGroupResponse_httpStatus' - The response's http status code.
newRemoveThingFromThingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveThingFromThingGroupResponse
newRemoveThingFromThingGroupResponse pHttpStatus_ =
  RemoveThingFromThingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeThingFromThingGroupResponse_httpStatus :: Lens.Lens' RemoveThingFromThingGroupResponse Prelude.Int
removeThingFromThingGroupResponse_httpStatus = Lens.lens (\RemoveThingFromThingGroupResponse' {httpStatus} -> httpStatus) (\s@RemoveThingFromThingGroupResponse' {} a -> s {httpStatus = a} :: RemoveThingFromThingGroupResponse)

instance
  Prelude.NFData
    RemoveThingFromThingGroupResponse
  where
  rnf RemoveThingFromThingGroupResponse' {..} =
    Prelude.rnf httpStatus
