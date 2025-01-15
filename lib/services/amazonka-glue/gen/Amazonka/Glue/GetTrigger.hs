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
-- Module      : Amazonka.Glue.GetTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
module Amazonka.Glue.GetTrigger
  ( -- * Creating a Request
    GetTrigger (..),
    newGetTrigger,

    -- * Request Lenses
    getTrigger_name,

    -- * Destructuring the Response
    GetTriggerResponse (..),
    newGetTriggerResponse,

    -- * Response Lenses
    getTriggerResponse_trigger,
    getTriggerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTrigger' smart constructor.
data GetTrigger = GetTrigger'
  { -- | The name of the trigger to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getTrigger_name' - The name of the trigger to retrieve.
newGetTrigger ::
  -- | 'name'
  Prelude.Text ->
  GetTrigger
newGetTrigger pName_ = GetTrigger' {name = pName_}

-- | The name of the trigger to retrieve.
getTrigger_name :: Lens.Lens' GetTrigger Prelude.Text
getTrigger_name = Lens.lens (\GetTrigger' {name} -> name) (\s@GetTrigger' {} a -> s {name = a} :: GetTrigger)

instance Core.AWSRequest GetTrigger where
  type AWSResponse GetTrigger = GetTriggerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTriggerResponse'
            Prelude.<$> (x Data..?> "Trigger")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrigger where
  hashWithSalt _salt GetTrigger' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetTrigger where
  rnf GetTrigger' {..} = Prelude.rnf name

instance Data.ToHeaders GetTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetTrigger" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTrigger where
  toJSON GetTrigger' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetTrigger where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
  { -- | The requested trigger definition.
    trigger :: Prelude.Maybe Trigger,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTriggerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trigger', 'getTriggerResponse_trigger' - The requested trigger definition.
--
-- 'httpStatus', 'getTriggerResponse_httpStatus' - The response's http status code.
newGetTriggerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTriggerResponse
newGetTriggerResponse pHttpStatus_ =
  GetTriggerResponse'
    { trigger = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested trigger definition.
getTriggerResponse_trigger :: Lens.Lens' GetTriggerResponse (Prelude.Maybe Trigger)
getTriggerResponse_trigger = Lens.lens (\GetTriggerResponse' {trigger} -> trigger) (\s@GetTriggerResponse' {} a -> s {trigger = a} :: GetTriggerResponse)

-- | The response's http status code.
getTriggerResponse_httpStatus :: Lens.Lens' GetTriggerResponse Prelude.Int
getTriggerResponse_httpStatus = Lens.lens (\GetTriggerResponse' {httpStatus} -> httpStatus) (\s@GetTriggerResponse' {} a -> s {httpStatus = a} :: GetTriggerResponse)

instance Prelude.NFData GetTriggerResponse where
  rnf GetTriggerResponse' {..} =
    Prelude.rnf trigger `Prelude.seq`
      Prelude.rnf httpStatus
