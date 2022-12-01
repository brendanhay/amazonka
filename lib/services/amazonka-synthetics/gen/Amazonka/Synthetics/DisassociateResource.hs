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
-- Module      : Amazonka.Synthetics.DisassociateResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a canary from a group. You must run this operation in the Region
-- where the canary exists.
module Amazonka.Synthetics.DisassociateResource
  ( -- * Creating a Request
    DisassociateResource (..),
    newDisassociateResource,

    -- * Request Lenses
    disassociateResource_groupIdentifier,
    disassociateResource_resourceArn,

    -- * Destructuring the Response
    DisassociateResourceResponse (..),
    newDisassociateResourceResponse,

    -- * Response Lenses
    disassociateResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Synthetics.Types

-- | /See:/ 'newDisassociateResource' smart constructor.
data DisassociateResource = DisassociateResource'
  { -- | Specifies the group. You can specify the group name, the ARN, or the
    -- group ID as the @GroupIdentifier@.
    groupIdentifier :: Prelude.Text,
    -- | The ARN of the canary that you want to remove from the specified group.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIdentifier', 'disassociateResource_groupIdentifier' - Specifies the group. You can specify the group name, the ARN, or the
-- group ID as the @GroupIdentifier@.
--
-- 'resourceArn', 'disassociateResource_resourceArn' - The ARN of the canary that you want to remove from the specified group.
newDisassociateResource ::
  -- | 'groupIdentifier'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  DisassociateResource
newDisassociateResource
  pGroupIdentifier_
  pResourceArn_ =
    DisassociateResource'
      { groupIdentifier =
          pGroupIdentifier_,
        resourceArn = pResourceArn_
      }

-- | Specifies the group. You can specify the group name, the ARN, or the
-- group ID as the @GroupIdentifier@.
disassociateResource_groupIdentifier :: Lens.Lens' DisassociateResource Prelude.Text
disassociateResource_groupIdentifier = Lens.lens (\DisassociateResource' {groupIdentifier} -> groupIdentifier) (\s@DisassociateResource' {} a -> s {groupIdentifier = a} :: DisassociateResource)

-- | The ARN of the canary that you want to remove from the specified group.
disassociateResource_resourceArn :: Lens.Lens' DisassociateResource Prelude.Text
disassociateResource_resourceArn = Lens.lens (\DisassociateResource' {resourceArn} -> resourceArn) (\s@DisassociateResource' {} a -> s {resourceArn = a} :: DisassociateResource)

instance Core.AWSRequest DisassociateResource where
  type
    AWSResponse DisassociateResource =
      DisassociateResourceResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateResource where
  hashWithSalt _salt DisassociateResource' {..} =
    _salt `Prelude.hashWithSalt` groupIdentifier
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DisassociateResource where
  rnf DisassociateResource' {..} =
    Prelude.rnf groupIdentifier
      `Prelude.seq` Prelude.rnf resourceArn

instance Core.ToHeaders DisassociateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateResource where
  toJSON DisassociateResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath DisassociateResource where
  toPath DisassociateResource' {..} =
    Prelude.mconcat
      [ "/group/",
        Core.toBS groupIdentifier,
        "/disassociate"
      ]

instance Core.ToQuery DisassociateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResourceResponse' smart constructor.
data DisassociateResourceResponse = DisassociateResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateResourceResponse_httpStatus' - The response's http status code.
newDisassociateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateResourceResponse
newDisassociateResourceResponse pHttpStatus_ =
  DisassociateResourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateResourceResponse_httpStatus :: Lens.Lens' DisassociateResourceResponse Prelude.Int
disassociateResourceResponse_httpStatus = Lens.lens (\DisassociateResourceResponse' {httpStatus} -> httpStatus) (\s@DisassociateResourceResponse' {} a -> s {httpStatus = a} :: DisassociateResourceResponse)

instance Prelude.NFData DisassociateResourceResponse where
  rnf DisassociateResourceResponse' {..} =
    Prelude.rnf httpStatus
