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
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a self-service action.
module Network.AWS.ServiceCatalog.DescribeServiceAction
  ( -- * Creating a Request
    DescribeServiceAction (..),
    newDescribeServiceAction,

    -- * Request Lenses
    describeServiceAction_acceptLanguage,
    describeServiceAction_id,

    -- * Destructuring the Response
    DescribeServiceActionResponse (..),
    newDescribeServiceActionResponse,

    -- * Response Lenses
    describeServiceActionResponse_serviceActionDetail,
    describeServiceActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeServiceAction' smart constructor.
data DescribeServiceAction = DescribeServiceAction'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeServiceAction_id' - The self-service action identifier.
newDescribeServiceAction ::
  -- | 'id'
  Prelude.Text ->
  DescribeServiceAction
newDescribeServiceAction pId_ =
  DescribeServiceAction'
    { acceptLanguage =
        Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeServiceAction_acceptLanguage :: Lens.Lens' DescribeServiceAction (Prelude.Maybe Prelude.Text)
describeServiceAction_acceptLanguage = Lens.lens (\DescribeServiceAction' {acceptLanguage} -> acceptLanguage) (\s@DescribeServiceAction' {} a -> s {acceptLanguage = a} :: DescribeServiceAction)

-- | The self-service action identifier.
describeServiceAction_id :: Lens.Lens' DescribeServiceAction Prelude.Text
describeServiceAction_id = Lens.lens (\DescribeServiceAction' {id} -> id) (\s@DescribeServiceAction' {} a -> s {id = a} :: DescribeServiceAction)

instance Prelude.AWSRequest DescribeServiceAction where
  type
    Rs DescribeServiceAction =
      DescribeServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceActionResponse'
            Prelude.<$> (x Prelude..?> "ServiceActionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceAction

instance Prelude.NFData DescribeServiceAction

instance Prelude.ToHeaders DescribeServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DescribeServiceAction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeServiceAction where
  toJSON DescribeServiceAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath DescribeServiceAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeServiceAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceActionResponse' smart constructor.
data DescribeServiceActionResponse = DescribeServiceActionResponse'
  { -- | Detailed information about the self-service action.
    serviceActionDetail :: Prelude.Maybe ServiceActionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionDetail', 'describeServiceActionResponse_serviceActionDetail' - Detailed information about the self-service action.
--
-- 'httpStatus', 'describeServiceActionResponse_httpStatus' - The response's http status code.
newDescribeServiceActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceActionResponse
newDescribeServiceActionResponse pHttpStatus_ =
  DescribeServiceActionResponse'
    { serviceActionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the self-service action.
describeServiceActionResponse_serviceActionDetail :: Lens.Lens' DescribeServiceActionResponse (Prelude.Maybe ServiceActionDetail)
describeServiceActionResponse_serviceActionDetail = Lens.lens (\DescribeServiceActionResponse' {serviceActionDetail} -> serviceActionDetail) (\s@DescribeServiceActionResponse' {} a -> s {serviceActionDetail = a} :: DescribeServiceActionResponse)

-- | The response's http status code.
describeServiceActionResponse_httpStatus :: Lens.Lens' DescribeServiceActionResponse Prelude.Int
describeServiceActionResponse_httpStatus = Lens.lens (\DescribeServiceActionResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceActionResponse' {} a -> s {httpStatus = a} :: DescribeServiceActionResponse)

instance Prelude.NFData DescribeServiceActionResponse
