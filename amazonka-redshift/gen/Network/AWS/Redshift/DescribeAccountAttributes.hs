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
-- Module      : Network.AWS.Redshift.DescribeAccountAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attributes attached to an account
module Network.AWS.Redshift.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Request Lenses
    describeAccountAttributes_attributeNames,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  { -- | A list of attribute names.
    attributeNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'describeAccountAttributes_attributeNames' - A list of attribute names.
newDescribeAccountAttributes ::
  DescribeAccountAttributes
newDescribeAccountAttributes =
  DescribeAccountAttributes'
    { attributeNames =
        Prelude.Nothing
    }

-- | A list of attribute names.
describeAccountAttributes_attributeNames :: Lens.Lens' DescribeAccountAttributes (Prelude.Maybe [Prelude.Text])
describeAccountAttributes_attributeNames = Lens.lens (\DescribeAccountAttributes' {attributeNames} -> attributeNames) (\s@DescribeAccountAttributes' {} a -> s {attributeNames = a} :: DescribeAccountAttributes) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Prelude.<$> ( x Prelude..@? "AccountAttributes"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may
                              (Prelude.parseXMLList "AccountAttribute")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountAttributes

instance Prelude.NFData DescribeAccountAttributes

instance Prelude.ToHeaders DescribeAccountAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeAccountAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAccountAttributes where
  toQuery DescribeAccountAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeAccountAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "AttributeNames"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "AttributeName"
                Prelude.<$> attributeNames
            )
      ]

-- | /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | A list of attributes assigned to an account.
    accountAttributes :: Prelude.Maybe [AccountAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAttributes', 'describeAccountAttributesResponse_accountAttributes' - A list of attributes assigned to an account.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { accountAttributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of attributes assigned to an account.
describeAccountAttributesResponse_accountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Prelude.Maybe [AccountAttribute])
describeAccountAttributesResponse_accountAttributes = Lens.lens (\DescribeAccountAttributesResponse' {accountAttributes} -> accountAttributes) (\s@DescribeAccountAttributesResponse' {} a -> s {accountAttributes = a} :: DescribeAccountAttributesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Prelude.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Prelude.NFData
    DescribeAccountAttributesResponse
