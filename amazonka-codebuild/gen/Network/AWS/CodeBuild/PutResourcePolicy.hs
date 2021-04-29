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
-- Module      : Network.AWS.CodeBuild.PutResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a resource policy for the ARN of a @Project@ or @ReportGroup@
-- object.
module Network.AWS.CodeBuild.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_resourceArn,
    putResourcePolicyResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | A JSON-formatted resource policy. For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project>
    -- and
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group>
    -- in the /AWS CodeBuild User Guide/.
    policy :: Prelude.Text,
    -- | The ARN of the @Project@ or @ReportGroup@ resource you want to associate
    -- with a resource policy.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putResourcePolicy_policy' - A JSON-formatted resource policy. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project>
-- and
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group>
-- in the /AWS CodeBuild User Guide/.
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The ARN of the @Project@ or @ReportGroup@ resource you want to associate
-- with a resource policy.
newPutResourcePolicy ::
  -- | 'policy'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pPolicy_ pResourceArn_ =
  PutResourcePolicy'
    { policy = pPolicy_,
      resourceArn = pResourceArn_
    }

-- | A JSON-formatted resource policy. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/project-sharing.html#project-sharing-share Sharing a Project>
-- and
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/report-groups-sharing.html#report-groups-sharing-share Sharing a Report Group>
-- in the /AWS CodeBuild User Guide/.
putResourcePolicy_policy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_policy = Lens.lens (\PutResourcePolicy' {policy} -> policy) (\s@PutResourcePolicy' {} a -> s {policy = a} :: PutResourcePolicy)

-- | The ARN of the @Project@ or @ReportGroup@ resource you want to associate
-- with a resource policy.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

instance Prelude.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (x Prelude..?> "resourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy

instance Prelude.NFData PutResourcePolicy

instance Prelude.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.PutResourcePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("policy" Prelude..= policy),
            Prelude.Just ("resourceArn" Prelude..= resourceArn)
          ]
      )

instance Prelude.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The ARN of the @Project@ or @ReportGroup@ resource that is associated
    -- with a resource policy.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putResourcePolicyResponse_resourceArn' - The ARN of the @Project@ or @ReportGroup@ resource that is associated
-- with a resource policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { resourceArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the @Project@ or @ReportGroup@ resource that is associated
-- with a resource policy.
putResourcePolicyResponse_resourceArn :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_resourceArn = Lens.lens (\PutResourcePolicyResponse' {resourceArn} -> resourceArn) (\s@PutResourcePolicyResponse' {} a -> s {resourceArn = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse
