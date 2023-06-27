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
-- Module      : Amazonka.ChimeSdkVoice.DeleteSipRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SIP rule.
module Amazonka.ChimeSdkVoice.DeleteSipRule
  ( -- * Creating a Request
    DeleteSipRule (..),
    newDeleteSipRule,

    -- * Request Lenses
    deleteSipRule_sipRuleId,

    -- * Destructuring the Response
    DeleteSipRuleResponse (..),
    newDeleteSipRuleResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSipRule' smart constructor.
data DeleteSipRule = DeleteSipRule'
  { -- | The SIP rule ID.
    sipRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSipRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipRuleId', 'deleteSipRule_sipRuleId' - The SIP rule ID.
newDeleteSipRule ::
  -- | 'sipRuleId'
  Prelude.Text ->
  DeleteSipRule
newDeleteSipRule pSipRuleId_ =
  DeleteSipRule' {sipRuleId = pSipRuleId_}

-- | The SIP rule ID.
deleteSipRule_sipRuleId :: Lens.Lens' DeleteSipRule Prelude.Text
deleteSipRule_sipRuleId = Lens.lens (\DeleteSipRule' {sipRuleId} -> sipRuleId) (\s@DeleteSipRule' {} a -> s {sipRuleId = a} :: DeleteSipRule)

instance Core.AWSRequest DeleteSipRule where
  type
    AWSResponse DeleteSipRule =
      DeleteSipRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteSipRuleResponse'

instance Prelude.Hashable DeleteSipRule where
  hashWithSalt _salt DeleteSipRule' {..} =
    _salt `Prelude.hashWithSalt` sipRuleId

instance Prelude.NFData DeleteSipRule where
  rnf DeleteSipRule' {..} = Prelude.rnf sipRuleId

instance Data.ToHeaders DeleteSipRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSipRule where
  toPath DeleteSipRule' {..} =
    Prelude.mconcat
      ["/sip-rules/", Data.toBS sipRuleId]

instance Data.ToQuery DeleteSipRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSipRuleResponse' smart constructor.
data DeleteSipRuleResponse = DeleteSipRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSipRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSipRuleResponse ::
  DeleteSipRuleResponse
newDeleteSipRuleResponse = DeleteSipRuleResponse'

instance Prelude.NFData DeleteSipRuleResponse where
  rnf _ = ()
