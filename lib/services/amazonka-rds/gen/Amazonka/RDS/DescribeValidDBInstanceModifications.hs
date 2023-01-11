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
-- Module      : Amazonka.RDS.DescribeValidDBInstanceModifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can call @DescribeValidDBInstanceModifications@ to learn what
-- modifications you can make to your DB instance. You can use this
-- information when you call @ModifyDBInstance@.
--
-- This command doesn\'t apply to RDS Custom.
module Amazonka.RDS.DescribeValidDBInstanceModifications
  ( -- * Creating a Request
    DescribeValidDBInstanceModifications (..),
    newDescribeValidDBInstanceModifications,

    -- * Request Lenses
    describeValidDBInstanceModifications_dbInstanceIdentifier,

    -- * Destructuring the Response
    DescribeValidDBInstanceModificationsResponse (..),
    newDescribeValidDBInstanceModificationsResponse,

    -- * Response Lenses
    describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage,
    describeValidDBInstanceModificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeValidDBInstanceModifications' smart constructor.
data DescribeValidDBInstanceModifications = DescribeValidDBInstanceModifications'
  { -- | The customer identifier or the ARN of your DB instance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeValidDBInstanceModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifier', 'describeValidDBInstanceModifications_dbInstanceIdentifier' - The customer identifier or the ARN of your DB instance.
newDescribeValidDBInstanceModifications ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  DescribeValidDBInstanceModifications
newDescribeValidDBInstanceModifications
  pDBInstanceIdentifier_ =
    DescribeValidDBInstanceModifications'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_
      }

-- | The customer identifier or the ARN of your DB instance.
describeValidDBInstanceModifications_dbInstanceIdentifier :: Lens.Lens' DescribeValidDBInstanceModifications Prelude.Text
describeValidDBInstanceModifications_dbInstanceIdentifier = Lens.lens (\DescribeValidDBInstanceModifications' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DescribeValidDBInstanceModifications' {} a -> s {dbInstanceIdentifier = a} :: DescribeValidDBInstanceModifications)

instance
  Core.AWSRequest
    DescribeValidDBInstanceModifications
  where
  type
    AWSResponse DescribeValidDBInstanceModifications =
      DescribeValidDBInstanceModificationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeValidDBInstanceModificationsResult"
      ( \s h x ->
          DescribeValidDBInstanceModificationsResponse'
            Prelude.<$> (x Data..@? "ValidDBInstanceModificationsMessage")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeValidDBInstanceModifications
  where
  hashWithSalt
    _salt
    DescribeValidDBInstanceModifications' {..} =
      _salt `Prelude.hashWithSalt` dbInstanceIdentifier

instance
  Prelude.NFData
    DescribeValidDBInstanceModifications
  where
  rnf DescribeValidDBInstanceModifications' {..} =
    Prelude.rnf dbInstanceIdentifier

instance
  Data.ToHeaders
    DescribeValidDBInstanceModifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeValidDBInstanceModifications
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeValidDBInstanceModifications
  where
  toQuery DescribeValidDBInstanceModifications' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeValidDBInstanceModifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newDescribeValidDBInstanceModificationsResponse' smart constructor.
data DescribeValidDBInstanceModificationsResponse = DescribeValidDBInstanceModificationsResponse'
  { validDBInstanceModificationsMessage :: Prelude.Maybe ValidDBInstanceModificationsMessage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeValidDBInstanceModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validDBInstanceModificationsMessage', 'describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage' - Undocumented member.
--
-- 'httpStatus', 'describeValidDBInstanceModificationsResponse_httpStatus' - The response's http status code.
newDescribeValidDBInstanceModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeValidDBInstanceModificationsResponse
newDescribeValidDBInstanceModificationsResponse
  pHttpStatus_ =
    DescribeValidDBInstanceModificationsResponse'
      { validDBInstanceModificationsMessage =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage :: Lens.Lens' DescribeValidDBInstanceModificationsResponse (Prelude.Maybe ValidDBInstanceModificationsMessage)
describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage = Lens.lens (\DescribeValidDBInstanceModificationsResponse' {validDBInstanceModificationsMessage} -> validDBInstanceModificationsMessage) (\s@DescribeValidDBInstanceModificationsResponse' {} a -> s {validDBInstanceModificationsMessage = a} :: DescribeValidDBInstanceModificationsResponse)

-- | The response's http status code.
describeValidDBInstanceModificationsResponse_httpStatus :: Lens.Lens' DescribeValidDBInstanceModificationsResponse Prelude.Int
describeValidDBInstanceModificationsResponse_httpStatus = Lens.lens (\DescribeValidDBInstanceModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeValidDBInstanceModificationsResponse' {} a -> s {httpStatus = a} :: DescribeValidDBInstanceModificationsResponse)

instance
  Prelude.NFData
    DescribeValidDBInstanceModificationsResponse
  where
  rnf DescribeValidDBInstanceModificationsResponse' {..} =
    Prelude.rnf validDBInstanceModificationsMessage
      `Prelude.seq` Prelude.rnf httpStatus
