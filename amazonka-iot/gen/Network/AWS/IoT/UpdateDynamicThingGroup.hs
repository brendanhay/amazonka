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
-- Module      : Network.AWS.IoT.UpdateDynamicThingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dynamic thing group.
module Network.AWS.IoT.UpdateDynamicThingGroup
  ( -- * Creating a Request
    UpdateDynamicThingGroup (..),
    newUpdateDynamicThingGroup,

    -- * Request Lenses
    updateDynamicThingGroup_queryString,
    updateDynamicThingGroup_expectedVersion,
    updateDynamicThingGroup_indexName,
    updateDynamicThingGroup_queryVersion,
    updateDynamicThingGroup_thingGroupName,
    updateDynamicThingGroup_thingGroupProperties,

    -- * Destructuring the Response
    UpdateDynamicThingGroupResponse (..),
    newUpdateDynamicThingGroupResponse,

    -- * Response Lenses
    updateDynamicThingGroupResponse_version,
    updateDynamicThingGroupResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDynamicThingGroup' smart constructor.
data UpdateDynamicThingGroup = UpdateDynamicThingGroup'
  { -- | The dynamic thing group search query string to update.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The expected version of the dynamic thing group to update.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The dynamic thing group index to update.
    --
    -- Currently one index is supported: \'AWS_Things\'.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The dynamic thing group query version to update.
    --
    -- Currently one query version is supported: \"2017-09-30\". If not
    -- specified, the query version defaults to this value.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the dynamic thing group to update.
    thingGroupName :: Prelude.Text,
    -- | The dynamic thing group properties to update.
    thingGroupProperties :: ThingGroupProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDynamicThingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryString', 'updateDynamicThingGroup_queryString' - The dynamic thing group search query string to update.
--
-- 'expectedVersion', 'updateDynamicThingGroup_expectedVersion' - The expected version of the dynamic thing group to update.
--
-- 'indexName', 'updateDynamicThingGroup_indexName' - The dynamic thing group index to update.
--
-- Currently one index is supported: \'AWS_Things\'.
--
-- 'queryVersion', 'updateDynamicThingGroup_queryVersion' - The dynamic thing group query version to update.
--
-- Currently one query version is supported: \"2017-09-30\". If not
-- specified, the query version defaults to this value.
--
-- 'thingGroupName', 'updateDynamicThingGroup_thingGroupName' - The name of the dynamic thing group to update.
--
-- 'thingGroupProperties', 'updateDynamicThingGroup_thingGroupProperties' - The dynamic thing group properties to update.
newUpdateDynamicThingGroup ::
  -- | 'thingGroupName'
  Prelude.Text ->
  -- | 'thingGroupProperties'
  ThingGroupProperties ->
  UpdateDynamicThingGroup
newUpdateDynamicThingGroup
  pThingGroupName_
  pThingGroupProperties_ =
    UpdateDynamicThingGroup'
      { queryString =
          Prelude.Nothing,
        expectedVersion = Prelude.Nothing,
        indexName = Prelude.Nothing,
        queryVersion = Prelude.Nothing,
        thingGroupName = pThingGroupName_,
        thingGroupProperties = pThingGroupProperties_
      }

-- | The dynamic thing group search query string to update.
updateDynamicThingGroup_queryString :: Lens.Lens' UpdateDynamicThingGroup (Prelude.Maybe Prelude.Text)
updateDynamicThingGroup_queryString = Lens.lens (\UpdateDynamicThingGroup' {queryString} -> queryString) (\s@UpdateDynamicThingGroup' {} a -> s {queryString = a} :: UpdateDynamicThingGroup)

-- | The expected version of the dynamic thing group to update.
updateDynamicThingGroup_expectedVersion :: Lens.Lens' UpdateDynamicThingGroup (Prelude.Maybe Prelude.Integer)
updateDynamicThingGroup_expectedVersion = Lens.lens (\UpdateDynamicThingGroup' {expectedVersion} -> expectedVersion) (\s@UpdateDynamicThingGroup' {} a -> s {expectedVersion = a} :: UpdateDynamicThingGroup)

-- | The dynamic thing group index to update.
--
-- Currently one index is supported: \'AWS_Things\'.
updateDynamicThingGroup_indexName :: Lens.Lens' UpdateDynamicThingGroup (Prelude.Maybe Prelude.Text)
updateDynamicThingGroup_indexName = Lens.lens (\UpdateDynamicThingGroup' {indexName} -> indexName) (\s@UpdateDynamicThingGroup' {} a -> s {indexName = a} :: UpdateDynamicThingGroup)

-- | The dynamic thing group query version to update.
--
-- Currently one query version is supported: \"2017-09-30\". If not
-- specified, the query version defaults to this value.
updateDynamicThingGroup_queryVersion :: Lens.Lens' UpdateDynamicThingGroup (Prelude.Maybe Prelude.Text)
updateDynamicThingGroup_queryVersion = Lens.lens (\UpdateDynamicThingGroup' {queryVersion} -> queryVersion) (\s@UpdateDynamicThingGroup' {} a -> s {queryVersion = a} :: UpdateDynamicThingGroup)

-- | The name of the dynamic thing group to update.
updateDynamicThingGroup_thingGroupName :: Lens.Lens' UpdateDynamicThingGroup Prelude.Text
updateDynamicThingGroup_thingGroupName = Lens.lens (\UpdateDynamicThingGroup' {thingGroupName} -> thingGroupName) (\s@UpdateDynamicThingGroup' {} a -> s {thingGroupName = a} :: UpdateDynamicThingGroup)

-- | The dynamic thing group properties to update.
updateDynamicThingGroup_thingGroupProperties :: Lens.Lens' UpdateDynamicThingGroup ThingGroupProperties
updateDynamicThingGroup_thingGroupProperties = Lens.lens (\UpdateDynamicThingGroup' {thingGroupProperties} -> thingGroupProperties) (\s@UpdateDynamicThingGroup' {} a -> s {thingGroupProperties = a} :: UpdateDynamicThingGroup)

instance Prelude.AWSRequest UpdateDynamicThingGroup where
  type
    Rs UpdateDynamicThingGroup =
      UpdateDynamicThingGroupResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDynamicThingGroupResponse'
            Prelude.<$> (x Prelude..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDynamicThingGroup

instance Prelude.NFData UpdateDynamicThingGroup

instance Prelude.ToHeaders UpdateDynamicThingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateDynamicThingGroup where
  toJSON UpdateDynamicThingGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("queryString" Prelude..=) Prelude.<$> queryString,
            ("expectedVersion" Prelude..=)
              Prelude.<$> expectedVersion,
            ("indexName" Prelude..=) Prelude.<$> indexName,
            ("queryVersion" Prelude..=) Prelude.<$> queryVersion,
            Prelude.Just
              ( "thingGroupProperties"
                  Prelude..= thingGroupProperties
              )
          ]
      )

instance Prelude.ToPath UpdateDynamicThingGroup where
  toPath UpdateDynamicThingGroup' {..} =
    Prelude.mconcat
      [ "/dynamic-thing-groups/",
        Prelude.toBS thingGroupName
      ]

instance Prelude.ToQuery UpdateDynamicThingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDynamicThingGroupResponse' smart constructor.
data UpdateDynamicThingGroupResponse = UpdateDynamicThingGroupResponse'
  { -- | The dynamic thing group version.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDynamicThingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateDynamicThingGroupResponse_version' - The dynamic thing group version.
--
-- 'httpStatus', 'updateDynamicThingGroupResponse_httpStatus' - The response's http status code.
newUpdateDynamicThingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDynamicThingGroupResponse
newUpdateDynamicThingGroupResponse pHttpStatus_ =
  UpdateDynamicThingGroupResponse'
    { version =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The dynamic thing group version.
updateDynamicThingGroupResponse_version :: Lens.Lens' UpdateDynamicThingGroupResponse (Prelude.Maybe Prelude.Integer)
updateDynamicThingGroupResponse_version = Lens.lens (\UpdateDynamicThingGroupResponse' {version} -> version) (\s@UpdateDynamicThingGroupResponse' {} a -> s {version = a} :: UpdateDynamicThingGroupResponse)

-- | The response's http status code.
updateDynamicThingGroupResponse_httpStatus :: Lens.Lens' UpdateDynamicThingGroupResponse Prelude.Int
updateDynamicThingGroupResponse_httpStatus = Lens.lens (\UpdateDynamicThingGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateDynamicThingGroupResponse' {} a -> s {httpStatus = a} :: UpdateDynamicThingGroupResponse)

instance
  Prelude.NFData
    UpdateDynamicThingGroupResponse
