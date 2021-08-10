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
-- Module      : Network.AWS.LexRuntime.Types.ActiveContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ActiveContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import qualified Network.AWS.Prelude as Prelude

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
    parameters :: Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)
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
activeContext_parameters = Lens.lens (\ActiveContext' {parameters} -> parameters) (\s@ActiveContext' {} a -> s {parameters = a} :: ActiveContext) Prelude.. Lens._Coerce

instance Core.FromJSON ActiveContext where
  parseJSON =
    Core.withObject
      "ActiveContext"
      ( \x ->
          ActiveContext'
            Prelude.<$> (x Core..: "name")
            Prelude.<*> (x Core..: "timeToLive")
            Prelude.<*> (x Core..:? "parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ActiveContext

instance Prelude.NFData ActiveContext

instance Core.ToJSON ActiveContext where
  toJSON ActiveContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("timeToLive" Core..= timeToLive),
            Prelude.Just ("parameters" Core..= parameters)
          ]
      )
