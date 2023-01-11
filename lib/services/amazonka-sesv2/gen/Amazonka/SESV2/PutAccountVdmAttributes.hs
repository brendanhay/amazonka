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
-- Module      : Amazonka.SESV2.PutAccountVdmAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update your Amazon SES account VDM attributes.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.PutAccountVdmAttributes
  ( -- * Creating a Request
    PutAccountVdmAttributes (..),
    newPutAccountVdmAttributes,

    -- * Request Lenses
    putAccountVdmAttributes_vdmAttributes,

    -- * Destructuring the Response
    PutAccountVdmAttributesResponse (..),
    newPutAccountVdmAttributesResponse,

    -- * Response Lenses
    putAccountVdmAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to submit new account VDM attributes.
--
-- /See:/ 'newPutAccountVdmAttributes' smart constructor.
data PutAccountVdmAttributes = PutAccountVdmAttributes'
  { -- | The VDM attributes that you wish to apply to your Amazon SES account.
    vdmAttributes :: VdmAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountVdmAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vdmAttributes', 'putAccountVdmAttributes_vdmAttributes' - The VDM attributes that you wish to apply to your Amazon SES account.
newPutAccountVdmAttributes ::
  -- | 'vdmAttributes'
  VdmAttributes ->
  PutAccountVdmAttributes
newPutAccountVdmAttributes pVdmAttributes_ =
  PutAccountVdmAttributes'
    { vdmAttributes =
        pVdmAttributes_
    }

-- | The VDM attributes that you wish to apply to your Amazon SES account.
putAccountVdmAttributes_vdmAttributes :: Lens.Lens' PutAccountVdmAttributes VdmAttributes
putAccountVdmAttributes_vdmAttributes = Lens.lens (\PutAccountVdmAttributes' {vdmAttributes} -> vdmAttributes) (\s@PutAccountVdmAttributes' {} a -> s {vdmAttributes = a} :: PutAccountVdmAttributes)

instance Core.AWSRequest PutAccountVdmAttributes where
  type
    AWSResponse PutAccountVdmAttributes =
      PutAccountVdmAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountVdmAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountVdmAttributes where
  hashWithSalt _salt PutAccountVdmAttributes' {..} =
    _salt `Prelude.hashWithSalt` vdmAttributes

instance Prelude.NFData PutAccountVdmAttributes where
  rnf PutAccountVdmAttributes' {..} =
    Prelude.rnf vdmAttributes

instance Data.ToHeaders PutAccountVdmAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountVdmAttributes where
  toJSON PutAccountVdmAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VdmAttributes" Data..= vdmAttributes)
          ]
      )

instance Data.ToPath PutAccountVdmAttributes where
  toPath = Prelude.const "/v2/email/account/vdm"

instance Data.ToQuery PutAccountVdmAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountVdmAttributesResponse' smart constructor.
data PutAccountVdmAttributesResponse = PutAccountVdmAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountVdmAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccountVdmAttributesResponse_httpStatus' - The response's http status code.
newPutAccountVdmAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountVdmAttributesResponse
newPutAccountVdmAttributesResponse pHttpStatus_ =
  PutAccountVdmAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAccountVdmAttributesResponse_httpStatus :: Lens.Lens' PutAccountVdmAttributesResponse Prelude.Int
putAccountVdmAttributesResponse_httpStatus = Lens.lens (\PutAccountVdmAttributesResponse' {httpStatus} -> httpStatus) (\s@PutAccountVdmAttributesResponse' {} a -> s {httpStatus = a} :: PutAccountVdmAttributesResponse)

instance
  Prelude.NFData
    PutAccountVdmAttributesResponse
  where
  rnf PutAccountVdmAttributesResponse' {..} =
    Prelude.rnf httpStatus
