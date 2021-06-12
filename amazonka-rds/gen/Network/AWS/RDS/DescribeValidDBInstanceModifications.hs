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
-- Module      : Network.AWS.RDS.DescribeValidDBInstanceModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can call @DescribeValidDBInstanceModifications@ to learn what
-- modifications you can make to your DB instance. You can use this
-- information when you call @ModifyDBInstance@.
module Network.AWS.RDS.DescribeValidDBInstanceModifications
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeValidDBInstanceModifications' smart constructor.
data DescribeValidDBInstanceModifications = DescribeValidDBInstanceModifications'
  { -- | The customer identifier or the ARN of your DB instance.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeValidDBInstanceModifications
newDescribeValidDBInstanceModifications
  pDBInstanceIdentifier_ =
    DescribeValidDBInstanceModifications'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_
      }

-- | The customer identifier or the ARN of your DB instance.
describeValidDBInstanceModifications_dbInstanceIdentifier :: Lens.Lens' DescribeValidDBInstanceModifications Core.Text
describeValidDBInstanceModifications_dbInstanceIdentifier = Lens.lens (\DescribeValidDBInstanceModifications' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DescribeValidDBInstanceModifications' {} a -> s {dbInstanceIdentifier = a} :: DescribeValidDBInstanceModifications)

instance
  Core.AWSRequest
    DescribeValidDBInstanceModifications
  where
  type
    AWSResponse DescribeValidDBInstanceModifications =
      DescribeValidDBInstanceModificationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeValidDBInstanceModificationsResult"
      ( \s h x ->
          DescribeValidDBInstanceModificationsResponse'
            Core.<$> (x Core..@? "ValidDBInstanceModificationsMessage")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeValidDBInstanceModifications

instance
  Core.NFData
    DescribeValidDBInstanceModifications

instance
  Core.ToHeaders
    DescribeValidDBInstanceModifications
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeValidDBInstanceModifications
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeValidDBInstanceModifications
  where
  toQuery DescribeValidDBInstanceModifications' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeValidDBInstanceModifications" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newDescribeValidDBInstanceModificationsResponse' smart constructor.
data DescribeValidDBInstanceModificationsResponse = DescribeValidDBInstanceModificationsResponse'
  { validDBInstanceModificationsMessage :: Core.Maybe ValidDBInstanceModificationsMessage,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeValidDBInstanceModificationsResponse
newDescribeValidDBInstanceModificationsResponse
  pHttpStatus_ =
    DescribeValidDBInstanceModificationsResponse'
      { validDBInstanceModificationsMessage =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage :: Lens.Lens' DescribeValidDBInstanceModificationsResponse (Core.Maybe ValidDBInstanceModificationsMessage)
describeValidDBInstanceModificationsResponse_validDBInstanceModificationsMessage = Lens.lens (\DescribeValidDBInstanceModificationsResponse' {validDBInstanceModificationsMessage} -> validDBInstanceModificationsMessage) (\s@DescribeValidDBInstanceModificationsResponse' {} a -> s {validDBInstanceModificationsMessage = a} :: DescribeValidDBInstanceModificationsResponse)

-- | The response's http status code.
describeValidDBInstanceModificationsResponse_httpStatus :: Lens.Lens' DescribeValidDBInstanceModificationsResponse Core.Int
describeValidDBInstanceModificationsResponse_httpStatus = Lens.lens (\DescribeValidDBInstanceModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeValidDBInstanceModificationsResponse' {} a -> s {httpStatus = a} :: DescribeValidDBInstanceModificationsResponse)

instance
  Core.NFData
    DescribeValidDBInstanceModificationsResponse
