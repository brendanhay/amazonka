{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexRuntime.Types.ActiveContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.ActiveContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexRuntime.Types.ActiveContextTimeToLive
import qualified Amazonka.Prelude as Prelude

-- | A context is a variable that contains information about the current
-- state of the conversation between a user and Amazon Lex. Context can be
-- set automatically by Amazon Lex when an intent is fulfilled, or it can
-- be set at runtime using the @PutContent@, @PutText@, or @PutSession@
-- operation.
--
-- /See:/ 'newActiveContext' smart constructor.
data ActiveContext = ActiveContext'
  { -- | The name of the context.
    name :: Prelude.Text,
    -- | The length of time or number of turns that a context remains active.
    timeToLive :: ActiveContextTimeToLive,
    -- | State variables for the current context. You can use these values as
    -- default values for slots in subsequent events.
    parameters :: Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'activeContext_name' - The name of the context.
--
-- 'timeToLive', 'activeContext_timeToLive' - The length of time or number of turns that a context remains active.
--
-- 'parameters', 'activeContext_parameters' - State variables for the current context. You can use these values as
-- default values for slots in subsequent events.
newActiveContext ::
  -- | 'name'
  Prelude.Text ->
  -- | 'timeToLive'
  ActiveContextTimeToLive ->
  ActiveContext
newActiveContext pName_ pTimeToLive_ =
  ActiveContext'
    { name = pName_,
      timeToLive = pTimeToLive_,
      parameters = Prelude.mempty
    }

-- | The name of the context.
activeContext_name :: Lens.Lens' ActiveContext Prelude.Text
activeContext_name = Lens.lens (\ActiveContext' {name} -> name) (\s@ActiveContext' {} a -> s {name = a} :: ActiveContext)

-- | The length of time or number of turns that a context remains active.
activeContext_timeToLive :: Lens.Lens' ActiveContext ActiveContextTimeToLive
activeContext_timeToLive = Lens.lens (\ActiveContext' {timeToLive} -> timeToLive) (\s@ActiveContext' {} a -> s {timeToLive = a} :: ActiveContext)

-- | State variables for the current context. You can use these values as
-- default values for slots in subsequent events.
activeContext_parameters :: Lens.Lens' ActiveContext (Prelude.HashMap Prelude.Text Prelude.Text)
activeContext_parameters = Lens.lens (\ActiveContext' {parameters} -> parameters) (\s@ActiveContext' {} a -> s {parameters = a} :: ActiveContext) Prelude.. Lens.coerced

instance Data.FromJSON ActiveContext where
  parseJSON =
    Data.withObject
      "ActiveContext"
      ( \x ->
          ActiveContext'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "timeToLive")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ActiveContext where
  hashWithSalt _salt ActiveContext' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` timeToLive
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData ActiveContext where
  rnf ActiveContext' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf timeToLive
      `Prelude.seq` Prelude.rnf parameters

instance Data.ToJSON ActiveContext where
  toJSON ActiveContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("timeToLive" Data..= timeToLive),
            Prelude.Just ("parameters" Data..= parameters)
          ]
      )
