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
-- Module      : Amazonka.CloudWatchEvents.ActivateEventSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a partner event source that has been deactivated. Once
-- activated, your matching event bus will start receiving events from the
-- event source.
module Amazonka.CloudWatchEvents.ActivateEventSource
  ( -- * Creating a Request
    ActivateEventSource (..),
    newActivateEventSource,

    -- * Request Lenses
    activateEventSource_name,

    -- * Destructuring the Response
    ActivateEventSourceResponse (..),
    newActivateEventSourceResponse,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newActivateEventSource' smart constructor.
data ActivateEventSource = ActivateEventSource'
  { -- | The name of the partner event source to activate.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'activateEventSource_name' - The name of the partner event source to activate.
newActivateEventSource ::
  -- | 'name'
  Prelude.Text ->
  ActivateEventSource
newActivateEventSource pName_ =
  ActivateEventSource' {name = pName_}

-- | The name of the partner event source to activate.
activateEventSource_name :: Lens.Lens' ActivateEventSource Prelude.Text
activateEventSource_name = Lens.lens (\ActivateEventSource' {name} -> name) (\s@ActivateEventSource' {} a -> s {name = a} :: ActivateEventSource)

instance Core.AWSRequest ActivateEventSource where
  type
    AWSResponse ActivateEventSource =
      ActivateEventSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ActivateEventSourceResponse'

instance Prelude.Hashable ActivateEventSource where
  hashWithSalt _salt ActivateEventSource' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ActivateEventSource where
  rnf ActivateEventSource' {..} = Prelude.rnf name

instance Data.ToHeaders ActivateEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.ActivateEventSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ActivateEventSource where
  toJSON ActivateEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath ActivateEventSource where
  toPath = Prelude.const "/"

instance Data.ToQuery ActivateEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateEventSourceResponse' smart constructor.
data ActivateEventSourceResponse = ActivateEventSourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newActivateEventSourceResponse ::
  ActivateEventSourceResponse
newActivateEventSourceResponse =
  ActivateEventSourceResponse'

instance Prelude.NFData ActivateEventSourceResponse where
  rnf _ = ()
