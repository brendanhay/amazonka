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
-- Module      : Amazonka.CodeGuruProfiler.PutPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds permissions to a profiling group\'s resource-based policy that are
-- provided using an action group. If a profiling group doesn\'t have a
-- resource-based policy, one is created for it using the permissions in
-- the action group and the roles and users in the @principals@ parameter.
--
-- >  <p> The one supported action group that can be added is <code>agentPermission</code> which grants <code>ConfigureAgent</code> and <code>PostAgent</code> permissions. For more information, see <a href="https://docs.aws.amazon.com/codeguru/latest/profiler-ug/resource-based-policies.html">Resource-based policies in CodeGuru Profiler</a> in the <i>Amazon CodeGuru Profiler User Guide</i>, <a href="https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ConfigureAgent.html"> <code>ConfigureAgent</code> </a>, and <a href="https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_PostAgentProfile.html"> <code>PostAgentProfile</code> </a>. </p> <p> The first time you call <code>PutPermission</code> on a profiling group, do not specify a <code>revisionId</code> because it doesn't have a resource-based policy. Subsequent calls must provide a <code>revisionId</code> to specify which revision of the resource-based policy to add the permissions to. </p> <p> The response contains the profiling group's JSON-formatted resource policy. </p>
module Amazonka.CodeGuruProfiler.PutPermission
  ( -- * Creating a Request
    PutPermission (..),
    newPutPermission,

    -- * Request Lenses
    putPermission_revisionId,
    putPermission_actionGroup,
    putPermission_principals,
    putPermission_profilingGroupName,

    -- * Destructuring the Response
    PutPermissionResponse (..),
    newPutPermissionResponse,

    -- * Response Lenses
    putPermissionResponse_httpStatus,
    putPermissionResponse_policy,
    putPermissionResponse_revisionId,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the @putPermissionRequest@.
--
-- /See:/ 'newPutPermission' smart constructor.
data PutPermission = PutPermission'
  { -- | A universally unique identifier (UUID) for the revision of the policy
    -- you are adding to the profiling group. Do not specify this when you add
    -- permissions to a profiling group for the first time. If a policy already
    -- exists on the profiling group, you must specify the @revisionId@.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies an action group that contains permissions to add to a
    -- profiling group resource. One action group is supported,
    -- @agentPermissions@, which grants permission to perform actions required
    -- by the profiling agent, @ConfigureAgent@ and @PostAgentProfile@
    -- permissions.
    actionGroup :: ActionGroup,
    -- | A list ARNs for the roles and users you want to grant access to the
    -- profiling group. Wildcards are not are supported in the ARNs.
    principals :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the profiling group to grant access to.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'putPermission_revisionId' - A universally unique identifier (UUID) for the revision of the policy
-- you are adding to the profiling group. Do not specify this when you add
-- permissions to a profiling group for the first time. If a policy already
-- exists on the profiling group, you must specify the @revisionId@.
--
-- 'actionGroup', 'putPermission_actionGroup' - Specifies an action group that contains permissions to add to a
-- profiling group resource. One action group is supported,
-- @agentPermissions@, which grants permission to perform actions required
-- by the profiling agent, @ConfigureAgent@ and @PostAgentProfile@
-- permissions.
--
-- 'principals', 'putPermission_principals' - A list ARNs for the roles and users you want to grant access to the
-- profiling group. Wildcards are not are supported in the ARNs.
--
-- 'profilingGroupName', 'putPermission_profilingGroupName' - The name of the profiling group to grant access to.
newPutPermission ::
  -- | 'actionGroup'
  ActionGroup ->
  -- | 'principals'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  PutPermission
newPutPermission
  pActionGroup_
  pPrincipals_
  pProfilingGroupName_ =
    PutPermission'
      { revisionId = Prelude.Nothing,
        actionGroup = pActionGroup_,
        principals = Lens.coerced Lens.# pPrincipals_,
        profilingGroupName = pProfilingGroupName_
      }

-- | A universally unique identifier (UUID) for the revision of the policy
-- you are adding to the profiling group. Do not specify this when you add
-- permissions to a profiling group for the first time. If a policy already
-- exists on the profiling group, you must specify the @revisionId@.
putPermission_revisionId :: Lens.Lens' PutPermission (Prelude.Maybe Prelude.Text)
putPermission_revisionId = Lens.lens (\PutPermission' {revisionId} -> revisionId) (\s@PutPermission' {} a -> s {revisionId = a} :: PutPermission)

-- | Specifies an action group that contains permissions to add to a
-- profiling group resource. One action group is supported,
-- @agentPermissions@, which grants permission to perform actions required
-- by the profiling agent, @ConfigureAgent@ and @PostAgentProfile@
-- permissions.
putPermission_actionGroup :: Lens.Lens' PutPermission ActionGroup
putPermission_actionGroup = Lens.lens (\PutPermission' {actionGroup} -> actionGroup) (\s@PutPermission' {} a -> s {actionGroup = a} :: PutPermission)

-- | A list ARNs for the roles and users you want to grant access to the
-- profiling group. Wildcards are not are supported in the ARNs.
putPermission_principals :: Lens.Lens' PutPermission (Prelude.NonEmpty Prelude.Text)
putPermission_principals = Lens.lens (\PutPermission' {principals} -> principals) (\s@PutPermission' {} a -> s {principals = a} :: PutPermission) Prelude.. Lens.coerced

-- | The name of the profiling group to grant access to.
putPermission_profilingGroupName :: Lens.Lens' PutPermission Prelude.Text
putPermission_profilingGroupName = Lens.lens (\PutPermission' {profilingGroupName} -> profilingGroupName) (\s@PutPermission' {} a -> s {profilingGroupName = a} :: PutPermission)

instance Core.AWSRequest PutPermission where
  type
    AWSResponse PutPermission =
      PutPermissionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPermissionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policy")
            Prelude.<*> (x Data..:> "revisionId")
      )

instance Prelude.Hashable PutPermission where
  hashWithSalt _salt PutPermission' {..} =
    _salt
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` actionGroup
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData PutPermission where
  rnf PutPermission' {..} =
    Prelude.rnf revisionId `Prelude.seq`
      Prelude.rnf actionGroup `Prelude.seq`
        Prelude.rnf principals `Prelude.seq`
          Prelude.rnf profilingGroupName

instance Data.ToHeaders PutPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPermission where
  toJSON PutPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("revisionId" Data..=) Prelude.<$> revisionId,
            Prelude.Just ("principals" Data..= principals)
          ]
      )

instance Data.ToPath PutPermission where
  toPath PutPermission' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/policy/",
        Data.toBS actionGroup
      ]

instance Data.ToQuery PutPermission where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the @putPermissionResponse@.
--
-- /See:/ 'newPutPermissionResponse' smart constructor.
data PutPermissionResponse = PutPermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The JSON-formatted resource-based policy on the profiling group that
    -- includes the added permissions.
    policy :: Prelude.Text,
    -- | A universally unique identifier (UUID) for the revision of the
    -- resource-based policy that includes the added permissions. The
    -- JSON-formatted policy is in the @policy@ element of the response.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putPermissionResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'putPermissionResponse_policy' - The JSON-formatted resource-based policy on the profiling group that
-- includes the added permissions.
--
-- 'revisionId', 'putPermissionResponse_revisionId' - A universally unique identifier (UUID) for the revision of the
-- resource-based policy that includes the added permissions. The
-- JSON-formatted policy is in the @policy@ element of the response.
newPutPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  PutPermissionResponse
newPutPermissionResponse
  pHttpStatus_
  pPolicy_
  pRevisionId_ =
    PutPermissionResponse'
      { httpStatus = pHttpStatus_,
        policy = pPolicy_,
        revisionId = pRevisionId_
      }

-- | The response's http status code.
putPermissionResponse_httpStatus :: Lens.Lens' PutPermissionResponse Prelude.Int
putPermissionResponse_httpStatus = Lens.lens (\PutPermissionResponse' {httpStatus} -> httpStatus) (\s@PutPermissionResponse' {} a -> s {httpStatus = a} :: PutPermissionResponse)

-- | The JSON-formatted resource-based policy on the profiling group that
-- includes the added permissions.
putPermissionResponse_policy :: Lens.Lens' PutPermissionResponse Prelude.Text
putPermissionResponse_policy = Lens.lens (\PutPermissionResponse' {policy} -> policy) (\s@PutPermissionResponse' {} a -> s {policy = a} :: PutPermissionResponse)

-- | A universally unique identifier (UUID) for the revision of the
-- resource-based policy that includes the added permissions. The
-- JSON-formatted policy is in the @policy@ element of the response.
putPermissionResponse_revisionId :: Lens.Lens' PutPermissionResponse Prelude.Text
putPermissionResponse_revisionId = Lens.lens (\PutPermissionResponse' {revisionId} -> revisionId) (\s@PutPermissionResponse' {} a -> s {revisionId = a} :: PutPermissionResponse)

instance Prelude.NFData PutPermissionResponse where
  rnf PutPermissionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf policy `Prelude.seq`
        Prelude.rnf revisionId
