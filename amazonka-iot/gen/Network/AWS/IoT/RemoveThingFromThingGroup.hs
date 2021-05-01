{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.RemoveThingFromThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IoT.RemoveThingFromThingGroup
  ( -- * Creating a Request
    RemoveThingFromThingGroup (..),
    newRemoveThingFromThingGroup,

    -- * Request Lenses
    removeThingFromThingGroup_thingArn,
    removeThingFromThingGroup_thingGroupArn,
    removeThingFromThingGroup_thingName,
    removeThingFromThingGroup_thingGroupName,

    -- * Destructuring the Response
    RemoveThingFromThingGroupResponse (..),
    newRemoveThingFromThingGroupResponse,

    -- * Response Lenses
    removeThingFromThingGroupResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveThingFromThingGroup' smart constructor.
data RemoveThingFromThingGroup = RemoveThingFromThingGroup'
  { -- | The ARN of the thing to remove from the group.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The group ARN.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing to remove from the group.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The group name.
    thingGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'thingName', 'removeThingFromThingGroup_thingName' - The name of the thing to remove from the group.
--
-- 'thingGroupName', 'removeThingFromThingGroup_thingGroupName' - The group name.
newRemoveThingFromThingGroup ::
  RemoveThingFromThingGroup
newRemoveThingFromThingGroup =
  RemoveThingFromThingGroup'
    { thingArn =
        Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      thingName = Prelude.Nothing,
      thingGroupName = Prelude.Nothing
    }

-- | The ARN of the thing to remove from the group.
removeThingFromThingGroup_thingArn :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingArn = Lens.lens (\RemoveThingFromThingGroup' {thingArn} -> thingArn) (\s@RemoveThingFromThingGroup' {} a -> s {thingArn = a} :: RemoveThingFromThingGroup)

-- | The group ARN.
removeThingFromThingGroup_thingGroupArn :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingGroupArn = Lens.lens (\RemoveThingFromThingGroup' {thingGroupArn} -> thingGroupArn) (\s@RemoveThingFromThingGroup' {} a -> s {thingGroupArn = a} :: RemoveThingFromThingGroup)

-- | The name of the thing to remove from the group.
removeThingFromThingGroup_thingName :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingName = Lens.lens (\RemoveThingFromThingGroup' {thingName} -> thingName) (\s@RemoveThingFromThingGroup' {} a -> s {thingName = a} :: RemoveThingFromThingGroup)

-- | The group name.
removeThingFromThingGroup_thingGroupName :: Lens.Lens' RemoveThingFromThingGroup (Prelude.Maybe Prelude.Text)
removeThingFromThingGroup_thingGroupName = Lens.lens (\RemoveThingFromThingGroup' {thingGroupName} -> thingGroupName) (\s@RemoveThingFromThingGroup' {} a -> s {thingGroupName = a} :: RemoveThingFromThingGroup)

instance Prelude.AWSRequest RemoveThingFromThingGroup where
  type
    Rs RemoveThingFromThingGroup =
      RemoveThingFromThingGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveThingFromThingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveThingFromThingGroup

instance Prelude.NFData RemoveThingFromThingGroup

instance Prelude.ToHeaders RemoveThingFromThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON RemoveThingFromThingGroup where
  toJSON RemoveThingFromThingGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("thingArn" Prelude..=) Prelude.<$> thingArn,
            ("thingGroupArn" Prelude..=)
              Prelude.<$> thingGroupArn,
            ("thingName" Prelude..=) Prelude.<$> thingName,
            ("thingGroupName" Prelude..=)
              Prelude.<$> thingGroupName
          ]
      )

instance Prelude.ToPath RemoveThingFromThingGroup where
  toPath =
    Prelude.const
      "/thing-groups/removeThingFromThingGroup"

instance Prelude.ToQuery RemoveThingFromThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveThingFromThingGroupResponse' smart constructor.
data RemoveThingFromThingGroupResponse = RemoveThingFromThingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
