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
-- Module      : Amazonka.ChimeSdkVoice.GetSipRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetSipRule
  ( -- * Creating a Request
    GetSipRule (..),
    newGetSipRule,

    -- * Request Lenses
    getSipRule_sipRuleId,

    -- * Destructuring the Response
    GetSipRuleResponse (..),
    newGetSipRuleResponse,

    -- * Response Lenses
    getSipRuleResponse_sipRule,
    getSipRuleResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSipRule' smart constructor.
data GetSipRule = GetSipRule'
  { sipRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipRuleId', 'getSipRule_sipRuleId' - Undocumented member.
newGetSipRule ::
  -- | 'sipRuleId'
  Prelude.Text ->
  GetSipRule
newGetSipRule pSipRuleId_ =
  GetSipRule' {sipRuleId = pSipRuleId_}

-- | Undocumented member.
getSipRule_sipRuleId :: Lens.Lens' GetSipRule Prelude.Text
getSipRule_sipRuleId = Lens.lens (\GetSipRule' {sipRuleId} -> sipRuleId) (\s@GetSipRule' {} a -> s {sipRuleId = a} :: GetSipRule)

instance Core.AWSRequest GetSipRule where
  type AWSResponse GetSipRule = GetSipRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSipRuleResponse'
            Prelude.<$> (x Data..?> "SipRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSipRule where
  hashWithSalt _salt GetSipRule' {..} =
    _salt `Prelude.hashWithSalt` sipRuleId

instance Prelude.NFData GetSipRule where
  rnf GetSipRule' {..} = Prelude.rnf sipRuleId

instance Data.ToHeaders GetSipRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSipRule where
  toPath GetSipRule' {..} =
    Prelude.mconcat
      ["/sip-rules/", Data.toBS sipRuleId]

instance Data.ToQuery GetSipRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSipRuleResponse' smart constructor.
data GetSipRuleResponse = GetSipRuleResponse'
  { sipRule :: Prelude.Maybe SipRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipRule', 'getSipRuleResponse_sipRule' - Undocumented member.
--
-- 'httpStatus', 'getSipRuleResponse_httpStatus' - The response's http status code.
newGetSipRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSipRuleResponse
newGetSipRuleResponse pHttpStatus_ =
  GetSipRuleResponse'
    { sipRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getSipRuleResponse_sipRule :: Lens.Lens' GetSipRuleResponse (Prelude.Maybe SipRule)
getSipRuleResponse_sipRule = Lens.lens (\GetSipRuleResponse' {sipRule} -> sipRule) (\s@GetSipRuleResponse' {} a -> s {sipRule = a} :: GetSipRuleResponse)

-- | The response's http status code.
getSipRuleResponse_httpStatus :: Lens.Lens' GetSipRuleResponse Prelude.Int
getSipRuleResponse_httpStatus = Lens.lens (\GetSipRuleResponse' {httpStatus} -> httpStatus) (\s@GetSipRuleResponse' {} a -> s {httpStatus = a} :: GetSipRuleResponse)

instance Prelude.NFData GetSipRuleResponse where
  rnf GetSipRuleResponse' {..} =
    Prelude.rnf sipRule
      `Prelude.seq` Prelude.rnf httpStatus
