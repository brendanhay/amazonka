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
-- Module      : Network.AWS.SNS.GetSMSAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the settings for sending SMS messages from your account.
--
-- These settings are set with the @SetSMSAttributes@ action.
module Network.AWS.SNS.GetSMSAttributes
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

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
getSMSAttributes_attributes = Lens.lens (\GetSMSAttributes' {attributes} -> attributes) (\s@GetSMSAttributes' {} a -> s {attributes = a} :: GetSMSAttributes) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest GetSMSAttributes where
  type
    AWSResponse GetSMSAttributes =
      GetSMSAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetSMSAttributesResult"
      ( \s h x ->
          GetSMSAttributesResponse'
            Prelude.<$> ( x Core..@? "attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSMSAttributes

instance Prelude.NFData GetSMSAttributes

instance Core.ToHeaders GetSMSAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetSMSAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSMSAttributes where
  toQuery GetSMSAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetSMSAttributes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "attributes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> attributes)
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
getSMSAttributesResponse_attributes = Lens.lens (\GetSMSAttributesResponse' {attributes} -> attributes) (\s@GetSMSAttributesResponse' {} a -> s {attributes = a} :: GetSMSAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSMSAttributesResponse_httpStatus :: Lens.Lens' GetSMSAttributesResponse Prelude.Int
getSMSAttributesResponse_httpStatus = Lens.lens (\GetSMSAttributesResponse' {httpStatus} -> httpStatus) (\s@GetSMSAttributesResponse' {} a -> s {httpStatus = a} :: GetSMSAttributesResponse)

instance Prelude.NFData GetSMSAttributesResponse
