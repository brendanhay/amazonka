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
-- Module      : Network.AWS.IoT.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy\'s
-- default (operative) version. This action affects all certificates to
-- which the policy is attached. To list the principals the policy is
-- attached to, use the ListPrincipalPolicy API.
module Network.AWS.IoT.SetDefaultPolicyVersion
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetDefaultPolicyVersion operation.
--
-- /See:/ 'newSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { -- | The policy name.
    policyName :: Prelude.Text,
    -- | The policy version ID.
    policyVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest SetDefaultPolicyVersion where
  type
    Rs SetDefaultPolicyVersion =
      SetDefaultPolicyVersionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull
      SetDefaultPolicyVersionResponse'

instance Prelude.Hashable SetDefaultPolicyVersion

instance Prelude.NFData SetDefaultPolicyVersion

instance Prelude.ToHeaders SetDefaultPolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON SetDefaultPolicyVersion where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath SetDefaultPolicyVersion where
  toPath SetDefaultPolicyVersion' {..} =
    Prelude.mconcat
      [ "/policies/",
        Prelude.toBS policyName,
        "/version/",
        Prelude.toBS policyVersionId
      ]

instance Prelude.ToQuery SetDefaultPolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
