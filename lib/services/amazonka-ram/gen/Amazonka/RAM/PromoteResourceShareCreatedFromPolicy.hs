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
-- Module      : Amazonka.RAM.PromoteResourceShareCreatedFromPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resource shares that were created by attaching a policy to a resource
-- are visible only to the resource share owner, and the resource share
-- cannot be modified in RAM.
--
-- Use this API action to promote the resource share. When you promote the
-- resource share, it becomes:
--
-- -   Visible to all principals that it is shared with.
--
-- -   Modifiable in RAM.
module Amazonka.RAM.PromoteResourceShareCreatedFromPolicy
  ( -- * Creating a Request
    PromoteResourceShareCreatedFromPolicy (..),
    newPromoteResourceShareCreatedFromPolicy,

    -- * Request Lenses
    promoteResourceShareCreatedFromPolicy_resourceShareArn,

    -- * Destructuring the Response
    PromoteResourceShareCreatedFromPolicyResponse (..),
    newPromoteResourceShareCreatedFromPolicyResponse,

    -- * Response Lenses
    promoteResourceShareCreatedFromPolicyResponse_returnValue,
    promoteResourceShareCreatedFromPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPromoteResourceShareCreatedFromPolicy' smart constructor.
data PromoteResourceShareCreatedFromPolicy = PromoteResourceShareCreatedFromPolicy'
  { -- | The Amazon Resource Name (ARN) of the resource share to promote.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromoteResourceShareCreatedFromPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceShareArn', 'promoteResourceShareCreatedFromPolicy_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share to promote.
newPromoteResourceShareCreatedFromPolicy ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  PromoteResourceShareCreatedFromPolicy
newPromoteResourceShareCreatedFromPolicy
  pResourceShareArn_ =
    PromoteResourceShareCreatedFromPolicy'
      { resourceShareArn =
          pResourceShareArn_
      }

-- | The Amazon Resource Name (ARN) of the resource share to promote.
promoteResourceShareCreatedFromPolicy_resourceShareArn :: Lens.Lens' PromoteResourceShareCreatedFromPolicy Prelude.Text
promoteResourceShareCreatedFromPolicy_resourceShareArn = Lens.lens (\PromoteResourceShareCreatedFromPolicy' {resourceShareArn} -> resourceShareArn) (\s@PromoteResourceShareCreatedFromPolicy' {} a -> s {resourceShareArn = a} :: PromoteResourceShareCreatedFromPolicy)

instance
  Core.AWSRequest
    PromoteResourceShareCreatedFromPolicy
  where
  type
    AWSResponse
      PromoteResourceShareCreatedFromPolicy =
      PromoteResourceShareCreatedFromPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PromoteResourceShareCreatedFromPolicyResponse'
            Prelude.<$> (x Core..?> "returnValue")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PromoteResourceShareCreatedFromPolicy
  where
  hashWithSalt
    _salt
    PromoteResourceShareCreatedFromPolicy' {..} =
      _salt `Prelude.hashWithSalt` resourceShareArn

instance
  Prelude.NFData
    PromoteResourceShareCreatedFromPolicy
  where
  rnf PromoteResourceShareCreatedFromPolicy' {..} =
    Prelude.rnf resourceShareArn

instance
  Core.ToHeaders
    PromoteResourceShareCreatedFromPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    PromoteResourceShareCreatedFromPolicy
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance
  Core.ToPath
    PromoteResourceShareCreatedFromPolicy
  where
  toPath =
    Prelude.const
      "/promoteresourcesharecreatedfrompolicy"

instance
  Core.ToQuery
    PromoteResourceShareCreatedFromPolicy
  where
  toQuery PromoteResourceShareCreatedFromPolicy' {..} =
    Prelude.mconcat
      ["resourceShareArn" Core.=: resourceShareArn]

-- | /See:/ 'newPromoteResourceShareCreatedFromPolicyResponse' smart constructor.
data PromoteResourceShareCreatedFromPolicyResponse = PromoteResourceShareCreatedFromPolicyResponse'
  { -- | Indicates whether the request succeeded.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromoteResourceShareCreatedFromPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'promoteResourceShareCreatedFromPolicyResponse_returnValue' - Indicates whether the request succeeded.
--
-- 'httpStatus', 'promoteResourceShareCreatedFromPolicyResponse_httpStatus' - The response's http status code.
newPromoteResourceShareCreatedFromPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PromoteResourceShareCreatedFromPolicyResponse
newPromoteResourceShareCreatedFromPolicyResponse
  pHttpStatus_ =
    PromoteResourceShareCreatedFromPolicyResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates whether the request succeeded.
promoteResourceShareCreatedFromPolicyResponse_returnValue :: Lens.Lens' PromoteResourceShareCreatedFromPolicyResponse (Prelude.Maybe Prelude.Bool)
promoteResourceShareCreatedFromPolicyResponse_returnValue = Lens.lens (\PromoteResourceShareCreatedFromPolicyResponse' {returnValue} -> returnValue) (\s@PromoteResourceShareCreatedFromPolicyResponse' {} a -> s {returnValue = a} :: PromoteResourceShareCreatedFromPolicyResponse)

-- | The response's http status code.
promoteResourceShareCreatedFromPolicyResponse_httpStatus :: Lens.Lens' PromoteResourceShareCreatedFromPolicyResponse Prelude.Int
promoteResourceShareCreatedFromPolicyResponse_httpStatus = Lens.lens (\PromoteResourceShareCreatedFromPolicyResponse' {httpStatus} -> httpStatus) (\s@PromoteResourceShareCreatedFromPolicyResponse' {} a -> s {httpStatus = a} :: PromoteResourceShareCreatedFromPolicyResponse)

instance
  Prelude.NFData
    PromoteResourceShareCreatedFromPolicyResponse
  where
  rnf
    PromoteResourceShareCreatedFromPolicyResponse' {..} =
      Prelude.rnf returnValue
        `Prelude.seq` Prelude.rnf httpStatus
