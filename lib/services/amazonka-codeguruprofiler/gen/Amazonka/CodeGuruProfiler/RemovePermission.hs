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
-- Module      : Amazonka.CodeGuruProfiler.RemovePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes permissions from a profiling group\'s resource-based policy that
-- are provided using an action group. The one supported action group that
-- can be removed is @agentPermission@ which grants @ConfigureAgent@ and
-- @PostAgent@ permissions. For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-ug/resource-based-policies.html Resource-based policies in CodeGuru Profiler>
-- in the /Amazon CodeGuru Profiler User Guide/,
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html ConfigureAgent>
-- , and
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_PostAgentProfile.html PostAgentProfile>
-- .
module Amazonka.CodeGuruProfiler.RemovePermission
  ( -- * Creating a Request
    RemovePermission (..),
    newRemovePermission,

    -- * Request Lenses
    removePermission_actionGroup,
    removePermission_profilingGroupName,
    removePermission_revisionId,

    -- * Destructuring the Response
    RemovePermissionResponse (..),
    newRemovePermissionResponse,

    -- * Response Lenses
    removePermissionResponse_httpStatus,
    removePermissionResponse_policy,
    removePermissionResponse_revisionId,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | >  The structure representing the <code>removePermissionRequest</code>.</p>
--
-- /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | Specifies an action group that contains the permissions to remove from a
    -- profiling group\'s resource-based policy. One action group is supported,
    -- @agentPermissions@, which grants @ConfigureAgent@ and @PostAgentProfile@
    -- permissions.
    actionGroup :: ActionGroup,
    -- | The name of the profiling group.
    profilingGroupName :: Prelude.Text,
    -- | A universally unique identifier (UUID) for the revision of the
    -- resource-based policy from which you want to remove permissions.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemovePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionGroup', 'removePermission_actionGroup' - Specifies an action group that contains the permissions to remove from a
-- profiling group\'s resource-based policy. One action group is supported,
-- @agentPermissions@, which grants @ConfigureAgent@ and @PostAgentProfile@
-- permissions.
--
-- 'profilingGroupName', 'removePermission_profilingGroupName' - The name of the profiling group.
--
-- 'revisionId', 'removePermission_revisionId' - A universally unique identifier (UUID) for the revision of the
-- resource-based policy from which you want to remove permissions.
newRemovePermission ::
  -- | 'actionGroup'
  ActionGroup ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  RemovePermission
newRemovePermission
  pActionGroup_
  pProfilingGroupName_
  pRevisionId_ =
    RemovePermission'
      { actionGroup = pActionGroup_,
        profilingGroupName = pProfilingGroupName_,
        revisionId = pRevisionId_
      }

-- | Specifies an action group that contains the permissions to remove from a
-- profiling group\'s resource-based policy. One action group is supported,
-- @agentPermissions@, which grants @ConfigureAgent@ and @PostAgentProfile@
-- permissions.
removePermission_actionGroup :: Lens.Lens' RemovePermission ActionGroup
removePermission_actionGroup = Lens.lens (\RemovePermission' {actionGroup} -> actionGroup) (\s@RemovePermission' {} a -> s {actionGroup = a} :: RemovePermission)

-- | The name of the profiling group.
removePermission_profilingGroupName :: Lens.Lens' RemovePermission Prelude.Text
removePermission_profilingGroupName = Lens.lens (\RemovePermission' {profilingGroupName} -> profilingGroupName) (\s@RemovePermission' {} a -> s {profilingGroupName = a} :: RemovePermission)

-- | A universally unique identifier (UUID) for the revision of the
-- resource-based policy from which you want to remove permissions.
removePermission_revisionId :: Lens.Lens' RemovePermission Prelude.Text
removePermission_revisionId = Lens.lens (\RemovePermission' {revisionId} -> revisionId) (\s@RemovePermission' {} a -> s {revisionId = a} :: RemovePermission)

instance Core.AWSRequest RemovePermission where
  type
    AWSResponse RemovePermission =
      RemovePermissionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemovePermissionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policy")
            Prelude.<*> (x Data..:> "revisionId")
      )

instance Prelude.Hashable RemovePermission where
  hashWithSalt _salt RemovePermission' {..} =
    _salt
      `Prelude.hashWithSalt` actionGroup
      `Prelude.hashWithSalt` profilingGroupName
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData RemovePermission where
  rnf RemovePermission' {..} =
    Prelude.rnf actionGroup `Prelude.seq`
      Prelude.rnf profilingGroupName `Prelude.seq`
        Prelude.rnf revisionId

instance Data.ToHeaders RemovePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemovePermission where
  toPath RemovePermission' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/policy/",
        Data.toBS actionGroup
      ]

instance Data.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Prelude.mconcat ["revisionId" Data.=: revisionId]

-- | The structure representing the @removePermissionResponse@.
--
-- /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The JSON-formatted resource-based policy on the profiling group after
    -- the specified permissions were removed.
    policy :: Prelude.Text,
    -- | A universally unique identifier (UUID) for the revision of the
    -- resource-based policy after the specified permissions were removed. The
    -- updated JSON-formatted policy is in the @policy@ element of the
    -- response.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removePermissionResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'removePermissionResponse_policy' - The JSON-formatted resource-based policy on the profiling group after
-- the specified permissions were removed.
--
-- 'revisionId', 'removePermissionResponse_revisionId' - A universally unique identifier (UUID) for the revision of the
-- resource-based policy after the specified permissions were removed. The
-- updated JSON-formatted policy is in the @policy@ element of the
-- response.
newRemovePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  RemovePermissionResponse
newRemovePermissionResponse
  pHttpStatus_
  pPolicy_
  pRevisionId_ =
    RemovePermissionResponse'
      { httpStatus =
          pHttpStatus_,
        policy = pPolicy_,
        revisionId = pRevisionId_
      }

-- | The response's http status code.
removePermissionResponse_httpStatus :: Lens.Lens' RemovePermissionResponse Prelude.Int
removePermissionResponse_httpStatus = Lens.lens (\RemovePermissionResponse' {httpStatus} -> httpStatus) (\s@RemovePermissionResponse' {} a -> s {httpStatus = a} :: RemovePermissionResponse)

-- | The JSON-formatted resource-based policy on the profiling group after
-- the specified permissions were removed.
removePermissionResponse_policy :: Lens.Lens' RemovePermissionResponse Prelude.Text
removePermissionResponse_policy = Lens.lens (\RemovePermissionResponse' {policy} -> policy) (\s@RemovePermissionResponse' {} a -> s {policy = a} :: RemovePermissionResponse)

-- | A universally unique identifier (UUID) for the revision of the
-- resource-based policy after the specified permissions were removed. The
-- updated JSON-formatted policy is in the @policy@ element of the
-- response.
removePermissionResponse_revisionId :: Lens.Lens' RemovePermissionResponse Prelude.Text
removePermissionResponse_revisionId = Lens.lens (\RemovePermissionResponse' {revisionId} -> revisionId) (\s@RemovePermissionResponse' {} a -> s {revisionId = a} :: RemovePermissionResponse)

instance Prelude.NFData RemovePermissionResponse where
  rnf RemovePermissionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf policy `Prelude.seq`
        Prelude.rnf revisionId
