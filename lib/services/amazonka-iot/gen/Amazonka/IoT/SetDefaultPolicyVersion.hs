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
-- Module      : Amazonka.IoT.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy\'s
-- default (operative) version. This action affects all certificates to
-- which the policy is attached. To list the principals the policy is
-- attached to, use the ListPrincipalPolicies action.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions SetDefaultPolicyVersion>
-- action.
module Amazonka.IoT.SetDefaultPolicyVersion
  ( -- * Creating a Request
    SetDefaultPolicyVersion (..),
    newSetDefaultPolicyVersion,

    -- * Request Lenses
    setDefaultPolicyVersion_policyName,
    setDefaultPolicyVersion_policyVersionId,

    -- * Destructuring the Response
    SetDefaultPolicyVersionResponse (..),
    newSetDefaultPolicyVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the SetDefaultPolicyVersion operation.
--
-- /See:/ 'newSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { -- | The policy name.
    policyName :: Prelude.Text,
    -- | The policy version ID.
    policyVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'setDefaultPolicyVersion_policyName' - The policy name.
--
-- 'policyVersionId', 'setDefaultPolicyVersion_policyVersionId' - The policy version ID.
newSetDefaultPolicyVersion ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Text ->
  SetDefaultPolicyVersion
newSetDefaultPolicyVersion
  pPolicyName_
  pPolicyVersionId_ =
    SetDefaultPolicyVersion'
      { policyName = pPolicyName_,
        policyVersionId = pPolicyVersionId_
      }

-- | The policy name.
setDefaultPolicyVersion_policyName :: Lens.Lens' SetDefaultPolicyVersion Prelude.Text
setDefaultPolicyVersion_policyName = Lens.lens (\SetDefaultPolicyVersion' {policyName} -> policyName) (\s@SetDefaultPolicyVersion' {} a -> s {policyName = a} :: SetDefaultPolicyVersion)

-- | The policy version ID.
setDefaultPolicyVersion_policyVersionId :: Lens.Lens' SetDefaultPolicyVersion Prelude.Text
setDefaultPolicyVersion_policyVersionId = Lens.lens (\SetDefaultPolicyVersion' {policyVersionId} -> policyVersionId) (\s@SetDefaultPolicyVersion' {} a -> s {policyVersionId = a} :: SetDefaultPolicyVersion)

instance Core.AWSRequest SetDefaultPolicyVersion where
  type
    AWSResponse SetDefaultPolicyVersion =
      SetDefaultPolicyVersionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull
      SetDefaultPolicyVersionResponse'

instance Prelude.Hashable SetDefaultPolicyVersion where
  hashWithSalt _salt SetDefaultPolicyVersion' {..} =
    _salt
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData SetDefaultPolicyVersion where
  rnf SetDefaultPolicyVersion' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyVersionId

instance Data.ToHeaders SetDefaultPolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SetDefaultPolicyVersion where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath SetDefaultPolicyVersion where
  toPath SetDefaultPolicyVersion' {..} =
    Prelude.mconcat
      [ "/policies/",
        Data.toBS policyName,
        "/version/",
        Data.toBS policyVersionId
      ]

instance Data.ToQuery SetDefaultPolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetDefaultPolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetDefaultPolicyVersionResponse ::
  SetDefaultPolicyVersionResponse
newSetDefaultPolicyVersionResponse =
  SetDefaultPolicyVersionResponse'

instance
  Prelude.NFData
    SetDefaultPolicyVersionResponse
  where
  rnf _ = ()
