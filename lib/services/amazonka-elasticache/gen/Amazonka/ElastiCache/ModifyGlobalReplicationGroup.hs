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
-- Module      : Amazonka.ElastiCache.ModifyGlobalReplicationGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a Global datastore.
module Amazonka.ElastiCache.ModifyGlobalReplicationGroup
  ( -- * Creating a Request
    ModifyGlobalReplicationGroup (..),
    newModifyGlobalReplicationGroup,

    -- * Request Lenses
    modifyGlobalReplicationGroup_globalReplicationGroupDescription,
    modifyGlobalReplicationGroup_cacheNodeType,
    modifyGlobalReplicationGroup_cacheParameterGroupName,
    modifyGlobalReplicationGroup_automaticFailoverEnabled,
    modifyGlobalReplicationGroup_engineVersion,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyGlobalReplicationGroup' smart constructor.
data ModifyGlobalReplicationGroup = ModifyGlobalReplicationGroup'
  { -- | A description of the Global datastore
    globalReplicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | A valid cache node type that you want to scale this Global datastore to.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache parameter group to use with the Global datastore.
    -- It must be compatible with the major engine version used by the Global
    -- datastore.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether a read replica is automatically promoted to
    -- read\/write primary if the existing primary encounters a failure.
    automaticFailoverEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The upgraded version of the cache engine to be run on the clusters in
    -- the Global datastore.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global datastore
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
-- 'globalReplicationGroupDescription', 'modifyGlobalReplicationGroup_globalReplicationGroupDescription' - A description of the Global datastore
--
-- 'cacheNodeType', 'modifyGlobalReplicationGroup_cacheNodeType' - A valid cache node type that you want to scale this Global datastore to.
--
-- 'cacheParameterGroupName', 'modifyGlobalReplicationGroup_cacheParameterGroupName' - The name of the cache parameter group to use with the Global datastore.
-- It must be compatible with the major engine version used by the Global
-- datastore.
--
-- 'automaticFailoverEnabled', 'modifyGlobalReplicationGroup_automaticFailoverEnabled' - Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
--
-- 'engineVersion', 'modifyGlobalReplicationGroup_engineVersion' - The upgraded version of the cache engine to be run on the clusters in
-- the Global datastore.
--
-- 'globalReplicationGroupId', 'modifyGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
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
      { globalReplicationGroupDescription =
          Prelude.Nothing,
        cacheNodeType = Prelude.Nothing,
        cacheParameterGroupName = Prelude.Nothing,
        automaticFailoverEnabled = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

-- | A description of the Global datastore
modifyGlobalReplicationGroup_globalReplicationGroupDescription :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_globalReplicationGroupDescription = Lens.lens (\ModifyGlobalReplicationGroup' {globalReplicationGroupDescription} -> globalReplicationGroupDescription) (\s@ModifyGlobalReplicationGroup' {} a -> s {globalReplicationGroupDescription = a} :: ModifyGlobalReplicationGroup)

-- | A valid cache node type that you want to scale this Global datastore to.
modifyGlobalReplicationGroup_cacheNodeType :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_cacheNodeType = Lens.lens (\ModifyGlobalReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ModifyGlobalReplicationGroup' {} a -> s {cacheNodeType = a} :: ModifyGlobalReplicationGroup)

-- | The name of the cache parameter group to use with the Global datastore.
-- It must be compatible with the major engine version used by the Global
-- datastore.
modifyGlobalReplicationGroup_cacheParameterGroupName :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_cacheParameterGroupName = Lens.lens (\ModifyGlobalReplicationGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ModifyGlobalReplicationGroup' {} a -> s {cacheParameterGroupName = a} :: ModifyGlobalReplicationGroup)

-- | Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
modifyGlobalReplicationGroup_automaticFailoverEnabled :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyGlobalReplicationGroup_automaticFailoverEnabled = Lens.lens (\ModifyGlobalReplicationGroup' {automaticFailoverEnabled} -> automaticFailoverEnabled) (\s@ModifyGlobalReplicationGroup' {} a -> s {automaticFailoverEnabled = a} :: ModifyGlobalReplicationGroup)

-- | The upgraded version of the cache engine to be run on the clusters in
-- the Global datastore.
modifyGlobalReplicationGroup_engineVersion :: Lens.Lens' ModifyGlobalReplicationGroup (Prelude.Maybe Prelude.Text)
modifyGlobalReplicationGroup_engineVersion = Lens.lens (\ModifyGlobalReplicationGroup' {engineVersion} -> engineVersion) (\s@ModifyGlobalReplicationGroup' {} a -> s {engineVersion = a} :: ModifyGlobalReplicationGroup)

-- | The name of the Global datastore
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalReplicationGroupResult"
      ( \s h x ->
          ModifyGlobalReplicationGroupResponse'
            Prelude.<$> (x Data..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyGlobalReplicationGroup
  where
  hashWithSalt _salt ModifyGlobalReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` globalReplicationGroupDescription
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` automaticFailoverEnabled
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalReplicationGroupId
      `Prelude.hashWithSalt` applyImmediately

instance Prelude.NFData ModifyGlobalReplicationGroup where
  rnf ModifyGlobalReplicationGroup' {..} =
    Prelude.rnf globalReplicationGroupDescription
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf automaticFailoverEnabled
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf applyImmediately

instance Data.ToHeaders ModifyGlobalReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyGlobalReplicationGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyGlobalReplicationGroup where
  toQuery ModifyGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyGlobalReplicationGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupDescription"
          Data.=: globalReplicationGroupDescription,
        "CacheNodeType" Data.=: cacheNodeType,
        "CacheParameterGroupName"
          Data.=: cacheParameterGroupName,
        "AutomaticFailoverEnabled"
          Data.=: automaticFailoverEnabled,
        "EngineVersion" Data.=: engineVersion,
        "GlobalReplicationGroupId"
          Data.=: globalReplicationGroupId,
        "ApplyImmediately" Data.=: applyImmediately
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
  where
  rnf ModifyGlobalReplicationGroupResponse' {..} =
    Prelude.rnf globalReplicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
