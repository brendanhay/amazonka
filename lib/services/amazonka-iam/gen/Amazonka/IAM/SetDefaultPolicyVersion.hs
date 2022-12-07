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
-- Module      : Amazonka.IAM.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy\'s
-- default (operative) version.
--
-- This operation affects all users, groups, and roles that the policy is
-- attached to. To list the users, groups, and roles that the policy is
-- attached to, use ListEntitiesForPolicy.
--
-- For information about managed policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Amazonka.IAM.SetDefaultPolicyVersion
  ( -- * Creating a Request
    SetDefaultPolicyVersion (..),
    newSetDefaultPolicyVersion,

    -- * Request Lenses
    setDefaultPolicyVersion_policyArn,
    setDefaultPolicyVersion_versionId,

    -- * Destructuring the Response
    SetDefaultPolicyVersionResponse (..),
    newSetDefaultPolicyVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { -- | The Amazon Resource Name (ARN) of the IAM policy whose default version
    -- you want to set.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policyArn :: Prelude.Text,
    -- | The version of the policy to set as the default (operative) version.
    --
    -- For more information about managed policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    versionId :: Prelude.Text
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
-- 'policyArn', 'setDefaultPolicyVersion_policyArn' - The Amazon Resource Name (ARN) of the IAM policy whose default version
-- you want to set.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'versionId', 'setDefaultPolicyVersion_versionId' - The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
newSetDefaultPolicyVersion ::
  -- | 'policyArn'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  SetDefaultPolicyVersion
newSetDefaultPolicyVersion pPolicyArn_ pVersionId_ =
  SetDefaultPolicyVersion'
    { policyArn = pPolicyArn_,
      versionId = pVersionId_
    }

-- | The Amazon Resource Name (ARN) of the IAM policy whose default version
-- you want to set.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
setDefaultPolicyVersion_policyArn :: Lens.Lens' SetDefaultPolicyVersion Prelude.Text
setDefaultPolicyVersion_policyArn = Lens.lens (\SetDefaultPolicyVersion' {policyArn} -> policyArn) (\s@SetDefaultPolicyVersion' {} a -> s {policyArn = a} :: SetDefaultPolicyVersion)

-- | The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
setDefaultPolicyVersion_versionId :: Lens.Lens' SetDefaultPolicyVersion Prelude.Text
setDefaultPolicyVersion_versionId = Lens.lens (\SetDefaultPolicyVersion' {versionId} -> versionId) (\s@SetDefaultPolicyVersion' {} a -> s {versionId = a} :: SetDefaultPolicyVersion)

instance Core.AWSRequest SetDefaultPolicyVersion where
  type
    AWSResponse SetDefaultPolicyVersion =
      SetDefaultPolicyVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      SetDefaultPolicyVersionResponse'

instance Prelude.Hashable SetDefaultPolicyVersion where
  hashWithSalt _salt SetDefaultPolicyVersion' {..} =
    _salt `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData SetDefaultPolicyVersion where
  rnf SetDefaultPolicyVersion' {..} =
    Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToHeaders SetDefaultPolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetDefaultPolicyVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery SetDefaultPolicyVersion where
  toQuery SetDefaultPolicyVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetDefaultPolicyVersion" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "PolicyArn" Data.=: policyArn,
        "VersionId" Data.=: versionId
      ]

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
