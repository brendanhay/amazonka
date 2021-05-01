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
-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @ListIdentities@ operation to list the email
-- addresses and domains associated with your account.
module Network.AWS.SES.ListVerifiedEmailAddresses
  ( -- * Creating a Request
    ListVerifiedEmailAddresses (..),
    newListVerifiedEmailAddresses,

    -- * Destructuring the Response
    ListVerifiedEmailAddressesResponse (..),
    newListVerifiedEmailAddressesResponse,

    -- * Response Lenses
    listVerifiedEmailAddressesResponse_verifiedEmailAddresses,
    listVerifiedEmailAddressesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newListVerifiedEmailAddresses' smart constructor.
data ListVerifiedEmailAddresses = ListVerifiedEmailAddresses'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListVerifiedEmailAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListVerifiedEmailAddresses ::
  ListVerifiedEmailAddresses
newListVerifiedEmailAddresses =
  ListVerifiedEmailAddresses'

instance
  Prelude.AWSRequest
    ListVerifiedEmailAddresses
  where
  type
    Rs ListVerifiedEmailAddresses =
      ListVerifiedEmailAddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListVerifiedEmailAddressesResult"
      ( \s h x ->
          ListVerifiedEmailAddressesResponse'
            Prelude.<$> ( x Prelude..@? "VerifiedEmailAddresses"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVerifiedEmailAddresses

instance Prelude.NFData ListVerifiedEmailAddresses

instance Prelude.ToHeaders ListVerifiedEmailAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListVerifiedEmailAddresses where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListVerifiedEmailAddresses where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ("ListVerifiedEmailAddresses" :: Prelude.ByteString),
            "Version"
              Prelude.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | A list of email addresses that you have verified with Amazon SES under
-- your AWS account.
--
-- /See:/ 'newListVerifiedEmailAddressesResponse' smart constructor.
data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse'
  { -- | A list of email addresses that have been verified.
    verifiedEmailAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListVerifiedEmailAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedEmailAddresses', 'listVerifiedEmailAddressesResponse_verifiedEmailAddresses' - A list of email addresses that have been verified.
--
-- 'httpStatus', 'listVerifiedEmailAddressesResponse_httpStatus' - The response's http status code.
newListVerifiedEmailAddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVerifiedEmailAddressesResponse
newListVerifiedEmailAddressesResponse pHttpStatus_ =
  ListVerifiedEmailAddressesResponse'
    { verifiedEmailAddresses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of email addresses that have been verified.
listVerifiedEmailAddressesResponse_verifiedEmailAddresses :: Lens.Lens' ListVerifiedEmailAddressesResponse (Prelude.Maybe [Prelude.Text])
listVerifiedEmailAddressesResponse_verifiedEmailAddresses = Lens.lens (\ListVerifiedEmailAddressesResponse' {verifiedEmailAddresses} -> verifiedEmailAddresses) (\s@ListVerifiedEmailAddressesResponse' {} a -> s {verifiedEmailAddresses = a} :: ListVerifiedEmailAddressesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listVerifiedEmailAddressesResponse_httpStatus :: Lens.Lens' ListVerifiedEmailAddressesResponse Prelude.Int
listVerifiedEmailAddressesResponse_httpStatus = Lens.lens (\ListVerifiedEmailAddressesResponse' {httpStatus} -> httpStatus) (\s@ListVerifiedEmailAddressesResponse' {} a -> s {httpStatus = a} :: ListVerifiedEmailAddressesResponse)

instance
  Prelude.NFData
    ListVerifiedEmailAddressesResponse
