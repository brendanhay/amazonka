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
-- Module      : Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a Global Datastore.
module Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
  ( -- * Creating a Request
    ModifyGlobalReplicationGroup (..),
    newModifyGlobalReplicationGroup,

    -- * Request Lenses
    modifyGlobalReplicationGroup_automaticFailoverEnabled,
    modifyGlobalReplicationGroup_cacheParameterGroupName,
    modifyGlobalReplicationGroup_engineVersion,
    modifyGlobalReplicationGroup_cacheNodeType,
    modifyGlobalReplicationGroup_globalReplicationGroupDescription,
    modifyGlobalReplicationGroup_globalReplicationGroupId,
    modifyGlobalReplicationGroup_applyImmediately,

    -- * Destructuring the Response
    ModifyGlobalReplicationGroupResponse (..),
    newModifyGlobalReplicationGroupResponse,

    -- * Response Lenses
    modifyGlobalReplicationGroupResponse_globalReplicationGroup,
    modifyGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyGlobalReplicationGroup' smart constructor.
data ModifyGlobalReplicationGroup = ModifyGlobalReplicationGroup'
  { -- | Determines whether a read replica is automatically promoted to
    -- read\/write primary if the existing primary encounters a failure.
    automaticFailoverEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the cache parameter group to use with the Global datastore.
    -- It must be compatible with the major engine version used by the Global
    -- datastore.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The upgraded version of the cache engine to be run on the clusters in
    -- the Global Datastore.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A valid cache node type that you want to scale this Global Datastore to.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | A description of the Global Datastore
    globalReplicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | This parameter causes the modifications in this request and any pending
    -- modifications to be applied, asynchronously and as soon as possible.
    -- Modifications to Global Replication Groups cannot be requested to be
    -- applied in PreferredMaintenceWindow.
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticFailoverEnabled', 'modifyGlobalReplicationGroup_automaticFailoverEnabled' - Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
--
-- 'cacheParameterGroupName', 'modifyGlobalReplicationGroup_cacheParameterGroupName' - The name of the cache parameter group to use with the Global datastore.
-- It must be compatible with the major engine version used by the Global
-- datastore.
--
-- 'engineVersion', 'modifyGlobalReplicationGroup_engineVersion' - The upgraded version of the cache engine to be run on the clusters in
-- the Global Datastore.
--
-- 'cacheNodeType', 'modifyGlobalReplicationGroup_cacheNodeType' - A valid cache node type that you want to scale this Global Datastore to.
--
-- 'globalReplicationGroupDescription', 'modifyGlobalReplicationGroup_globalReplicationGroupDescription' - A description of the Global Datastore
--
-- 'globalReplicationGroupId', 'modifyGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'applyImmediately', 'modifyGlobalReplicationGroup_applyImmediately' - This parameter causes the modifications in this request and any pending
-- modifications to be applied, asynchronously and as soon as possible.
-- Modifications to Global Replication Groups cannot be requested to be
-- applied in PreferredMaintenceWindow.
newModifyGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Prelude.Text ->
  -- | 'applyImmediately'
  Prelude.Bool ->
  ModifyGlobalReplicationGroup
newModifyGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    ModifyGlobalReplicationGroup'
      { automaticFailoverEnabled =
          Prelude.Nothing,
        cacheParameterGroupName = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        cacheNodeType = Prelude.Nothing,
        globalReplicationGroupDescription =
          Prelude.Nothing,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

-- | Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
modifyGlobalReplicationGroup_automaticFailoverEnabled :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyGlobalReplicationGroup_automaticFailoverEnabled = Lens.lens (\ModifyGlobalReplicationGroup' {automaticFailoverEnabled} -> automaticFailoverEnabled) (\s@ModifyGlobalReplicationGroup' {} a -> s {automaticFailoverEnabled = a} :: ModifyGlobalReplicationGroup)

-- | The name of the cache parameter group to use with the Global datastore.
-- It must be compatible with the major engine version used by the Global
-- datastore.
modifyGlobalReplicationGroup_cacheParameterGroupName :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_cacheParameterGroupName = Lens.lens (\ModifyGlobalReplicationGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ModifyGlobalReplicationGroup' {} a -> s {cacheParameterGroupName = a} :: ModifyGlobalReplicationGroup)

-- | The upgraded version of the cache engine to be run on the clusters in
-- the Global Datastore.
modifyGlobalReplicationGroup_engineVersion :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_engineVersion = Lens.lens (\ModifyGlobalReplicationGroup' {engineVersion} -> engineVersion) (\s@ModifyGlobalReplicationGroup' {} a -> s {engineVersion = a} :: ModifyGlobalReplicationGroup)

-- | A valid cache node type that you want to scale this Global Datastore to.
modifyGlobalReplicationGroup_cacheNodeType :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_cacheNodeType = Lens.lens (\ModifyGlobalReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ModifyGlobalReplicationGroup' {} a -> s {cacheNodeType = a} :: ModifyGlobalReplicationGroup)

-- | A description of the Global Datastore
modifyGlobalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\ModifyGlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@ModifyGlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: ModifyGlobalReplicationGroup)

-- | The name of the Global Datastore
modifyGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' ModifyGlobalReplicationGroup Prelude.Text
modifyGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\ModifyGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@ModifyGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: ModifyGlobalReplicationGroup)

-- | This parameter causes the modifications in this request and any pending
-- modifications to be applied, asynchronously and as soon as possible.
-- Modifications to Global Replication Groups cannot be requested to be
-- applied in PreferredMaintenceWindow.
modifyGlobalReplicationGroup_applyImmediately :: Lens.Lens' ModifyGlobalReplicationGroup Prelude.Bool
modifyGlobalReplicationGroup_applyImmediately = Lens.lens (\ModifyGlobalReplicationGroup' {applyImmediately} -> applyImmediately) (\s@ModifyGlobalReplicationGroup' {} a -> s {applyImmediately = a} :: ModifyGlobalReplicationGroup)

instance Core.AWSRequest ModifyGlobalReplicationGroup where
  type
    AWSResponse ModifyGlobalReplicationGroup =
      ModifyGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalReplicationGroupResult"
      ( \s h x ->
          ModifyGlobalReplicationGroupResponse'
            Prelude.<$> (x Core..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyGlobalReplicationGroup

instance Prelude.NFData ModifyGlobalReplicationGroup

instance Core.ToHeaders ModifyGlobalReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyGlobalReplicationGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyGlobalReplicationGroup where
  toQuery ModifyGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "AutomaticFailoverEnabled"
          Core.=: automaticFailoverEnabled,
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName,
        "EngineVersion" Core.=: engineVersion,
        "CacheNodeType" Core.=: cacheNodeType,
        "GlobalReplicationGroupDescription"
          Core.=: globalReplicationGroupDescription,
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "ApplyImmediately" Core.=: applyImmediately
      ]

-- | /See:/ 'newModifyGlobalReplicationGroupResponse' smart constructor.
data ModifyGlobalReplicationGroupResponse = ModifyGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'modifyGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newModifyGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyGlobalReplicationGroupResponse
newModifyGlobalReplicationGroupResponse pHttpStatus_ =
  ModifyGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' ModifyGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
modifyGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\ModifyGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@ModifyGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: ModifyGlobalReplicationGroupResponse)

-- | The response's http status code.
modifyGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' ModifyGlobalReplicationGroupResponse Prelude.Int
modifyGlobalReplicationGroupResponse_httpStatus = Lens.lens (\ModifyGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: ModifyGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    ModifyGlobalReplicationGroupResponse
