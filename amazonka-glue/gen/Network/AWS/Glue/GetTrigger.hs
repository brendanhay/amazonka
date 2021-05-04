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
-- Module      : Network.AWS.Glue.GetTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
module Network.AWS.Glue.GetTrigger
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTrigger' smart constructor.
data GetTrigger = GetTrigger'
  { -- | The name of the trigger to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetTrigger where
  type Rs GetTrigger = GetTriggerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTriggerResponse'
            Prelude.<$> (x Prelude..?> "Trigger")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrigger

instance Prelude.NFData GetTrigger

instance Prelude.ToHeaders GetTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetTrigger" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetTrigger where
  toJSON GetTrigger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath GetTrigger where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
  { -- | The requested trigger definition.
    trigger :: Prelude.Maybe Trigger,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetTriggerResponse
