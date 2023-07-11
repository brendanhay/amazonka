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
-- Module      : Amazonka.CloudWatch.PutManagedInsightRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed Contributor Insights rule for a specified Amazon Web
-- Services resource. When you enable a managed rule, you create a
-- Contributor Insights rule that collects data from Amazon Web Services
-- services. You cannot edit these rules with @PutInsightRule@. The rules
-- can be enabled, disabled, and deleted using @EnableInsightRules@,
-- @DisableInsightRules@, and @DeleteInsightRules@. If a previously created
-- managed rule is currently disabled, a subsequent call to this API will
-- re-enable it. Use @ListManagedInsightRules@ to describe all available
-- rules.
module Amazonka.CloudWatch.PutManagedInsightRules
  ( -- * Creating a Request
    PutManagedInsightRules (..),
    newPutManagedInsightRules,

    -- * Request Lenses
    putManagedInsightRules_managedRules,

    -- * Destructuring the Response
    PutManagedInsightRulesResponse (..),
    newPutManagedInsightRulesResponse,

    -- * Response Lenses
    putManagedInsightRulesResponse_failures,
    putManagedInsightRulesResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutManagedInsightRules' smart constructor.
data PutManagedInsightRules = PutManagedInsightRules'
  { -- | A list of @ManagedRules@ to enable.
    managedRules :: [ManagedRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutManagedInsightRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedRules', 'putManagedInsightRules_managedRules' - A list of @ManagedRules@ to enable.
newPutManagedInsightRules ::
  PutManagedInsightRules
newPutManagedInsightRules =
  PutManagedInsightRules'
    { managedRules =
        Prelude.mempty
    }

-- | A list of @ManagedRules@ to enable.
putManagedInsightRules_managedRules :: Lens.Lens' PutManagedInsightRules [ManagedRule]
putManagedInsightRules_managedRules = Lens.lens (\PutManagedInsightRules' {managedRules} -> managedRules) (\s@PutManagedInsightRules' {} a -> s {managedRules = a} :: PutManagedInsightRules) Prelude.. Lens.coerced

instance Core.AWSRequest PutManagedInsightRules where
  type
    AWSResponse PutManagedInsightRules =
      PutManagedInsightRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutManagedInsightRulesResult"
      ( \s h x ->
          PutManagedInsightRulesResponse'
            Prelude.<$> ( x
                            Data..@? "Failures"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutManagedInsightRules where
  hashWithSalt _salt PutManagedInsightRules' {..} =
    _salt `Prelude.hashWithSalt` managedRules

instance Prelude.NFData PutManagedInsightRules where
  rnf PutManagedInsightRules' {..} =
    Prelude.rnf managedRules

instance Data.ToHeaders PutManagedInsightRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutManagedInsightRules where
  toPath = Prelude.const "/"

instance Data.ToQuery PutManagedInsightRules where
  toQuery PutManagedInsightRules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutManagedInsightRules" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "ManagedRules"
          Data.=: Data.toQueryList "member" managedRules
      ]

-- | /See:/ 'newPutManagedInsightRulesResponse' smart constructor.
data PutManagedInsightRulesResponse = PutManagedInsightRulesResponse'
  { -- | An array that lists the rules that could not be enabled.
    failures :: Prelude.Maybe [PartialFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutManagedInsightRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'putManagedInsightRulesResponse_failures' - An array that lists the rules that could not be enabled.
--
-- 'httpStatus', 'putManagedInsightRulesResponse_httpStatus' - The response's http status code.
newPutManagedInsightRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutManagedInsightRulesResponse
newPutManagedInsightRulesResponse pHttpStatus_ =
  PutManagedInsightRulesResponse'
    { failures =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that lists the rules that could not be enabled.
putManagedInsightRulesResponse_failures :: Lens.Lens' PutManagedInsightRulesResponse (Prelude.Maybe [PartialFailure])
putManagedInsightRulesResponse_failures = Lens.lens (\PutManagedInsightRulesResponse' {failures} -> failures) (\s@PutManagedInsightRulesResponse' {} a -> s {failures = a} :: PutManagedInsightRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putManagedInsightRulesResponse_httpStatus :: Lens.Lens' PutManagedInsightRulesResponse Prelude.Int
putManagedInsightRulesResponse_httpStatus = Lens.lens (\PutManagedInsightRulesResponse' {httpStatus} -> httpStatus) (\s@PutManagedInsightRulesResponse' {} a -> s {httpStatus = a} :: PutManagedInsightRulesResponse)

instance
  Prelude.NFData
    PutManagedInsightRulesResponse
  where
  rnf PutManagedInsightRulesResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
