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
-- Module      : Amazonka.Lightsail.IsVpcPeered
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a Boolean value indicating whether your Lightsail VPC is peered.
module Amazonka.Lightsail.IsVpcPeered
  ( -- * Creating a Request
    IsVpcPeered (..),
    newIsVpcPeered,

    -- * Destructuring the Response
    IsVpcPeeredResponse (..),
    newIsVpcPeeredResponse,

    -- * Response Lenses
    isVpcPeeredResponse_isPeered,
    isVpcPeeredResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newIsVpcPeered' smart constructor.
data IsVpcPeered = IsVpcPeered'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsVpcPeered' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newIsVpcPeered ::
  IsVpcPeered
newIsVpcPeered = IsVpcPeered'

instance Core.AWSRequest IsVpcPeered where
  type AWSResponse IsVpcPeered = IsVpcPeeredResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          IsVpcPeeredResponse'
            Prelude.<$> (x Data..?> "isPeered")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IsVpcPeered where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData IsVpcPeered where
  rnf _ = ()

instance Data.ToHeaders IsVpcPeered where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.IsVpcPeered" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON IsVpcPeered where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath IsVpcPeered where
  toPath = Prelude.const "/"

instance Data.ToQuery IsVpcPeered where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIsVpcPeeredResponse' smart constructor.
data IsVpcPeeredResponse = IsVpcPeeredResponse'
  { -- | Returns @true@ if the Lightsail VPC is peered; otherwise, @false@.
    isPeered :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IsVpcPeeredResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPeered', 'isVpcPeeredResponse_isPeered' - Returns @true@ if the Lightsail VPC is peered; otherwise, @false@.
--
-- 'httpStatus', 'isVpcPeeredResponse_httpStatus' - The response's http status code.
newIsVpcPeeredResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  IsVpcPeeredResponse
newIsVpcPeeredResponse pHttpStatus_ =
  IsVpcPeeredResponse'
    { isPeered = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the Lightsail VPC is peered; otherwise, @false@.
isVpcPeeredResponse_isPeered :: Lens.Lens' IsVpcPeeredResponse (Prelude.Maybe Prelude.Bool)
isVpcPeeredResponse_isPeered = Lens.lens (\IsVpcPeeredResponse' {isPeered} -> isPeered) (\s@IsVpcPeeredResponse' {} a -> s {isPeered = a} :: IsVpcPeeredResponse)

-- | The response's http status code.
isVpcPeeredResponse_httpStatus :: Lens.Lens' IsVpcPeeredResponse Prelude.Int
isVpcPeeredResponse_httpStatus = Lens.lens (\IsVpcPeeredResponse' {httpStatus} -> httpStatus) (\s@IsVpcPeeredResponse' {} a -> s {httpStatus = a} :: IsVpcPeeredResponse)

instance Prelude.NFData IsVpcPeeredResponse where
  rnf IsVpcPeeredResponse' {..} =
    Prelude.rnf isPeered
      `Prelude.seq` Prelude.rnf httpStatus
