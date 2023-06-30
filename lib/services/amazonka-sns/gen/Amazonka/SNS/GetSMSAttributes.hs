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
-- Module      : Amazonka.SNS.GetSMSAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the settings for sending SMS messages from your Amazon Web
-- Services account.
--
-- These settings are set with the @SetSMSAttributes@ action.
module Amazonka.SNS.GetSMSAttributes
  ( -- * Creating a Request
    GetSMSAttributes (..),
    newGetSMSAttributes,

    -- * Request Lenses
    getSMSAttributes_attributes,

    -- * Destructuring the Response
    GetSMSAttributesResponse (..),
    newGetSMSAttributesResponse,

    -- * Response Lenses
    getSMSAttributesResponse_attributes,
    getSMSAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | The input for the @GetSMSAttributes@ request.
--
-- /See:/ 'newGetSMSAttributes' smart constructor.
data GetSMSAttributes = GetSMSAttributes'
  { -- | A list of the individual attribute names, such as @MonthlySpendLimit@,
    -- for which you want values.
    --
    -- For all attribute names, see
    -- <https://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes>.
    --
    -- If you don\'t use this parameter, Amazon SNS returns all SMS attributes.
    attributes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSMSAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getSMSAttributes_attributes' - A list of the individual attribute names, such as @MonthlySpendLimit@,
-- for which you want values.
--
-- For all attribute names, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes>.
--
-- If you don\'t use this parameter, Amazon SNS returns all SMS attributes.
newGetSMSAttributes ::
  GetSMSAttributes
newGetSMSAttributes =
  GetSMSAttributes' {attributes = Prelude.Nothing}

-- | A list of the individual attribute names, such as @MonthlySpendLimit@,
-- for which you want values.
--
-- For all attribute names, see
-- <https://docs.aws.amazon.com/sns/latest/api/API_SetSMSAttributes.html SetSMSAttributes>.
--
-- If you don\'t use this parameter, Amazon SNS returns all SMS attributes.
getSMSAttributes_attributes :: Lens.Lens' GetSMSAttributes (Prelude.Maybe [Prelude.Text])
getSMSAttributes_attributes = Lens.lens (\GetSMSAttributes' {attributes} -> attributes) (\s@GetSMSAttributes' {} a -> s {attributes = a} :: GetSMSAttributes) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest GetSMSAttributes where
  type
    AWSResponse GetSMSAttributes =
      GetSMSAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetSMSAttributesResult"
      ( \s h x ->
          GetSMSAttributesResponse'
            Prelude.<$> ( x
                            Data..@? "attributes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSMSAttributes where
  hashWithSalt _salt GetSMSAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributes

instance Prelude.NFData GetSMSAttributes where
  rnf GetSMSAttributes' {..} = Prelude.rnf attributes

instance Data.ToHeaders GetSMSAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSMSAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSMSAttributes where
  toQuery GetSMSAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetSMSAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "attributes"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> attributes)
      ]

-- | The response from the @GetSMSAttributes@ request.
--
-- /See:/ 'newGetSMSAttributesResponse' smart constructor.
data GetSMSAttributesResponse = GetSMSAttributesResponse'
  { -- | The SMS attribute names and their values.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSMSAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getSMSAttributesResponse_attributes' - The SMS attribute names and their values.
--
-- 'httpStatus', 'getSMSAttributesResponse_httpStatus' - The response's http status code.
newGetSMSAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSMSAttributesResponse
newGetSMSAttributesResponse pHttpStatus_ =
  GetSMSAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The SMS attribute names and their values.
getSMSAttributesResponse_attributes :: Lens.Lens' GetSMSAttributesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSMSAttributesResponse_attributes = Lens.lens (\GetSMSAttributesResponse' {attributes} -> attributes) (\s@GetSMSAttributesResponse' {} a -> s {attributes = a} :: GetSMSAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSMSAttributesResponse_httpStatus :: Lens.Lens' GetSMSAttributesResponse Prelude.Int
getSMSAttributesResponse_httpStatus = Lens.lens (\GetSMSAttributesResponse' {httpStatus} -> httpStatus) (\s@GetSMSAttributesResponse' {} a -> s {httpStatus = a} :: GetSMSAttributesResponse)

instance Prelude.NFData GetSMSAttributesResponse where
  rnf GetSMSAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
