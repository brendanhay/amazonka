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
-- Module      : Network.AWS.LexV2Runtime.Types.ActiveContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.ActiveContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.ActiveContextTimeToLive
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the contexts that a user is using in a
-- session. You can configure Amazon Lex V2 to set a context when an intent
-- is fulfilled, or you can set a context using the , , or operations.
--
-- Use a context to indicate to Amazon Lex V2 intents that should be used
-- as follow-up intents. For example, if the active context is
-- @order-fulfilled@, only intents that have @order-fulfilled@ configured
-- as a trigger are considered for follow up.
--
-- /See:/ 'newActiveContext' smart constructor.
data ActiveContext = ActiveContext'
  { -- | The name of the context.
    name :: Prelude.Text,
    -- | Indicates the number of turns or seconds that the context is active.
    -- Once the time to live expires, the context is no longer returned in a
    -- response.
    timeToLive :: ActiveContextTimeToLive,
    -- | A list of contexts active for the request. A context can be activated
    -- when a previous intent is fulfilled, or by including the context in the
    -- request.
    --
    -- If you don\'t specify a list of contexts, Amazon Lex V2 will use the
    -- current list of contexts for the session. If you specify an empty list,
    -- all contexts for the session are cleared.
    contextAttributes :: Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)
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
-- 'timeToLive', 'activeContext_timeToLive' - Indicates the number of turns or seconds that the context is active.
-- Once the time to live expires, the context is no longer returned in a
-- response.
--
-- 'contextAttributes', 'activeContext_contextAttributes' - A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request.
--
-- If you don\'t specify a list of contexts, Amazon Lex V2 will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
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
      contextAttributes = Prelude.mempty
    }

-- | The name of the context.
activeContext_name :: Lens.Lens' ActiveContext Prelude.Text
activeContext_name = Lens.lens (\ActiveContext' {name} -> name) (\s@ActiveContext' {} a -> s {name = a} :: ActiveContext)

-- | Indicates the number of turns or seconds that the context is active.
-- Once the time to live expires, the context is no longer returned in a
-- response.
activeContext_timeToLive :: Lens.Lens' ActiveContext ActiveContextTimeToLive
activeContext_timeToLive = Lens.lens (\ActiveContext' {timeToLive} -> timeToLive) (\s@ActiveContext' {} a -> s {timeToLive = a} :: ActiveContext)

-- | A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request.
--
-- If you don\'t specify a list of contexts, Amazon Lex V2 will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
activeContext_contextAttributes :: Lens.Lens' ActiveContext (Prelude.HashMap Prelude.Text Prelude.Text)
activeContext_contextAttributes = Lens.lens (\ActiveContext' {contextAttributes} -> contextAttributes) (\s@ActiveContext' {} a -> s {contextAttributes = a} :: ActiveContext) Prelude.. Lens.coerced

instance Core.FromJSON ActiveContext where
  parseJSON =
    Core.withObject
      "ActiveContext"
      ( \x ->
          ActiveContext'
            Prelude.<$> (x Core..: "name")
            Prelude.<*> (x Core..: "timeToLive")
            Prelude.<*> ( x Core..:? "contextAttributes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ActiveContext

instance Prelude.NFData ActiveContext

instance Core.ToJSON ActiveContext where
  toJSON ActiveContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("timeToLive" Core..= timeToLive),
            Prelude.Just
              ("contextAttributes" Core..= contextAttributes)
          ]
      )
