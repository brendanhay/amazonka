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
-- Module      : Amazonka.IoT.DescribeAuditSuppression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit suppression.
module Amazonka.IoT.DescribeAuditSuppression
  ( -- * Creating a Request
    DescribeAuditSuppression (..),
    newDescribeAuditSuppression,

    -- * Request Lenses
    describeAuditSuppression_checkName,
    describeAuditSuppression_resourceIdentifier,

    -- * Destructuring the Response
    DescribeAuditSuppressionResponse (..),
    newDescribeAuditSuppressionResponse,

    -- * Response Lenses
    describeAuditSuppressionResponse_checkName,
    describeAuditSuppressionResponse_description,
    describeAuditSuppressionResponse_expirationDate,
    describeAuditSuppressionResponse_resourceIdentifier,
    describeAuditSuppressionResponse_suppressIndefinitely,
    describeAuditSuppressionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAuditSuppression' smart constructor.
data DescribeAuditSuppression = DescribeAuditSuppression'
  { checkName :: Prelude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkName', 'describeAuditSuppression_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'describeAuditSuppression_resourceIdentifier' - Undocumented member.
newDescribeAuditSuppression ::
  -- | 'checkName'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  DescribeAuditSuppression
newDescribeAuditSuppression
  pCheckName_
  pResourceIdentifier_ =
    DescribeAuditSuppression'
      { checkName = pCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | Undocumented member.
describeAuditSuppression_checkName :: Lens.Lens' DescribeAuditSuppression Prelude.Text
describeAuditSuppression_checkName = Lens.lens (\DescribeAuditSuppression' {checkName} -> checkName) (\s@DescribeAuditSuppression' {} a -> s {checkName = a} :: DescribeAuditSuppression)

-- | Undocumented member.
describeAuditSuppression_resourceIdentifier :: Lens.Lens' DescribeAuditSuppression ResourceIdentifier
describeAuditSuppression_resourceIdentifier = Lens.lens (\DescribeAuditSuppression' {resourceIdentifier} -> resourceIdentifier) (\s@DescribeAuditSuppression' {} a -> s {resourceIdentifier = a} :: DescribeAuditSuppression)

instance Core.AWSRequest DescribeAuditSuppression where
  type
    AWSResponse DescribeAuditSuppression =
      DescribeAuditSuppressionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAuditSuppressionResponse'
            Prelude.<$> (x Data..?> "checkName")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "expirationDate")
            Prelude.<*> (x Data..?> "resourceIdentifier")
            Prelude.<*> (x Data..?> "suppressIndefinitely")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAuditSuppression where
  hashWithSalt _salt DescribeAuditSuppression' {..} =
    _salt `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData DescribeAuditSuppression where
  rnf DescribeAuditSuppression' {..} =
    Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToHeaders DescribeAuditSuppression where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribeAuditSuppression where
  toJSON DescribeAuditSuppression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("checkName" Data..= checkName),
            Prelude.Just
              ("resourceIdentifier" Data..= resourceIdentifier)
          ]
      )

instance Data.ToPath DescribeAuditSuppression where
  toPath = Prelude.const "/audit/suppressions/describe"

instance Data.ToQuery DescribeAuditSuppression where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAuditSuppressionResponse' smart constructor.
data DescribeAuditSuppressionResponse = DescribeAuditSuppressionResponse'
  { checkName :: Prelude.Maybe Prelude.Text,
    -- | The description of the audit suppression.
    description :: Prelude.Maybe Prelude.Text,
    -- | The epoch timestamp in seconds at which this suppression expires.
    expirationDate :: Prelude.Maybe Data.POSIX,
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier,
    -- | Indicates whether a suppression should exist indefinitely or not.
    suppressIndefinitely :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuditSuppressionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkName', 'describeAuditSuppressionResponse_checkName' - Undocumented member.
--
-- 'description', 'describeAuditSuppressionResponse_description' - The description of the audit suppression.
--
-- 'expirationDate', 'describeAuditSuppressionResponse_expirationDate' - The epoch timestamp in seconds at which this suppression expires.
--
-- 'resourceIdentifier', 'describeAuditSuppressionResponse_resourceIdentifier' - Undocumented member.
--
-- 'suppressIndefinitely', 'describeAuditSuppressionResponse_suppressIndefinitely' - Indicates whether a suppression should exist indefinitely or not.
--
-- 'httpStatus', 'describeAuditSuppressionResponse_httpStatus' - The response's http status code.
newDescribeAuditSuppressionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuditSuppressionResponse
newDescribeAuditSuppressionResponse pHttpStatus_ =
  DescribeAuditSuppressionResponse'
    { checkName =
        Prelude.Nothing,
      description = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      suppressIndefinitely = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeAuditSuppressionResponse_checkName :: Lens.Lens' DescribeAuditSuppressionResponse (Prelude.Maybe Prelude.Text)
describeAuditSuppressionResponse_checkName = Lens.lens (\DescribeAuditSuppressionResponse' {checkName} -> checkName) (\s@DescribeAuditSuppressionResponse' {} a -> s {checkName = a} :: DescribeAuditSuppressionResponse)

-- | The description of the audit suppression.
describeAuditSuppressionResponse_description :: Lens.Lens' DescribeAuditSuppressionResponse (Prelude.Maybe Prelude.Text)
describeAuditSuppressionResponse_description = Lens.lens (\DescribeAuditSuppressionResponse' {description} -> description) (\s@DescribeAuditSuppressionResponse' {} a -> s {description = a} :: DescribeAuditSuppressionResponse)

-- | The epoch timestamp in seconds at which this suppression expires.
describeAuditSuppressionResponse_expirationDate :: Lens.Lens' DescribeAuditSuppressionResponse (Prelude.Maybe Prelude.UTCTime)
describeAuditSuppressionResponse_expirationDate = Lens.lens (\DescribeAuditSuppressionResponse' {expirationDate} -> expirationDate) (\s@DescribeAuditSuppressionResponse' {} a -> s {expirationDate = a} :: DescribeAuditSuppressionResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describeAuditSuppressionResponse_resourceIdentifier :: Lens.Lens' DescribeAuditSuppressionResponse (Prelude.Maybe ResourceIdentifier)
describeAuditSuppressionResponse_resourceIdentifier = Lens.lens (\DescribeAuditSuppressionResponse' {resourceIdentifier} -> resourceIdentifier) (\s@DescribeAuditSuppressionResponse' {} a -> s {resourceIdentifier = a} :: DescribeAuditSuppressionResponse)

-- | Indicates whether a suppression should exist indefinitely or not.
describeAuditSuppressionResponse_suppressIndefinitely :: Lens.Lens' DescribeAuditSuppressionResponse (Prelude.Maybe Prelude.Bool)
describeAuditSuppressionResponse_suppressIndefinitely = Lens.lens (\DescribeAuditSuppressionResponse' {suppressIndefinitely} -> suppressIndefinitely) (\s@DescribeAuditSuppressionResponse' {} a -> s {suppressIndefinitely = a} :: DescribeAuditSuppressionResponse)

-- | The response's http status code.
describeAuditSuppressionResponse_httpStatus :: Lens.Lens' DescribeAuditSuppressionResponse Prelude.Int
describeAuditSuppressionResponse_httpStatus = Lens.lens (\DescribeAuditSuppressionResponse' {httpStatus} -> httpStatus) (\s@DescribeAuditSuppressionResponse' {} a -> s {httpStatus = a} :: DescribeAuditSuppressionResponse)

instance
  Prelude.NFData
    DescribeAuditSuppressionResponse
  where
  rnf DescribeAuditSuppressionResponse' {..} =
    Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf suppressIndefinitely
      `Prelude.seq` Prelude.rnf httpStatus
