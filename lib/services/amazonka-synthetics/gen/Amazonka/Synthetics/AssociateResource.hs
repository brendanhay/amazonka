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
-- Module      : Amazonka.Synthetics.AssociateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a canary with a group. Using groups can help you with
-- managing and automating your canaries, and you can also view aggregated
-- run results and statistics for all canaries in a group.
--
-- You must run this operation in the Region where the canary exists.
module Amazonka.Synthetics.AssociateResource
  ( -- * Creating a Request
    AssociateResource (..),
    newAssociateResource,

    -- * Request Lenses
    associateResource_groupIdentifier,
    associateResource_resourceArn,

    -- * Destructuring the Response
    AssociateResourceResponse (..),
    newAssociateResourceResponse,

    -- * Response Lenses
    associateResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newAssociateResource' smart constructor.
data AssociateResource = AssociateResource'
  { -- | Specifies the group. You can specify the group name, the ARN, or the
    -- group ID as the @GroupIdentifier@.
    groupIdentifier :: Prelude.Text,
    -- | The ARN of the canary that you want to associate with the specified
    -- group.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIdentifier', 'associateResource_groupIdentifier' - Specifies the group. You can specify the group name, the ARN, or the
-- group ID as the @GroupIdentifier@.
--
-- 'resourceArn', 'associateResource_resourceArn' - The ARN of the canary that you want to associate with the specified
-- group.
newAssociateResource ::
  -- | 'groupIdentifier'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  AssociateResource
newAssociateResource pGroupIdentifier_ pResourceArn_ =
  AssociateResource'
    { groupIdentifier =
        pGroupIdentifier_,
      resourceArn = pResourceArn_
    }

-- | Specifies the group. You can specify the group name, the ARN, or the
-- group ID as the @GroupIdentifier@.
associateResource_groupIdentifier :: Lens.Lens' AssociateResource Prelude.Text
associateResource_groupIdentifier = Lens.lens (\AssociateResource' {groupIdentifier} -> groupIdentifier) (\s@AssociateResource' {} a -> s {groupIdentifier = a} :: AssociateResource)

-- | The ARN of the canary that you want to associate with the specified
-- group.
associateResource_resourceArn :: Lens.Lens' AssociateResource Prelude.Text
associateResource_resourceArn = Lens.lens (\AssociateResource' {resourceArn} -> resourceArn) (\s@AssociateResource' {} a -> s {resourceArn = a} :: AssociateResource)

instance Core.AWSRequest AssociateResource where
  type
    AWSResponse AssociateResource =
      AssociateResourceResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateResource where
  hashWithSalt _salt AssociateResource' {..} =
    _salt
      `Prelude.hashWithSalt` groupIdentifier
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData AssociateResource where
  rnf AssociateResource' {..} =
    Prelude.rnf groupIdentifier
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders AssociateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateResource where
  toJSON AssociateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath AssociateResource where
  toPath AssociateResource' {..} =
    Prelude.mconcat
      ["/group/", Data.toBS groupIdentifier, "/associate"]

instance Data.ToQuery AssociateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResourceResponse' smart constructor.
data AssociateResourceResponse = AssociateResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateResourceResponse_httpStatus' - The response's http status code.
newAssociateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateResourceResponse
newAssociateResourceResponse pHttpStatus_ =
  AssociateResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateResourceResponse_httpStatus :: Lens.Lens' AssociateResourceResponse Prelude.Int
associateResourceResponse_httpStatus = Lens.lens (\AssociateResourceResponse' {httpStatus} -> httpStatus) (\s@AssociateResourceResponse' {} a -> s {httpStatus = a} :: AssociateResourceResponse)

instance Prelude.NFData AssociateResourceResponse where
  rnf AssociateResourceResponse' {..} =
    Prelude.rnf httpStatus
