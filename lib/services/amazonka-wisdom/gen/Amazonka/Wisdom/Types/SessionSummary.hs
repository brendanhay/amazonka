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
-- Module      : Amazonka.Wisdom.Types.SessionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.SessionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about the session.
--
-- /See:/ 'newSessionSummary' smart constructor.
data SessionSummary = SessionSummary'
  { -- | The Amazon Resource Name (ARN) of the Wisdom assistant.
    assistantArn :: Prelude.Text,
    -- | The identifier of the Wisdom assistant.
    assistantId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the session.
    sessionArn :: Prelude.Text,
    -- | The identifier of the session.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantArn', 'sessionSummary_assistantArn' - The Amazon Resource Name (ARN) of the Wisdom assistant.
--
-- 'assistantId', 'sessionSummary_assistantId' - The identifier of the Wisdom assistant.
--
-- 'sessionArn', 'sessionSummary_sessionArn' - The Amazon Resource Name (ARN) of the session.
--
-- 'sessionId', 'sessionSummary_sessionId' - The identifier of the session.
newSessionSummary ::
  -- | 'assistantArn'
  Prelude.Text ->
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'sessionArn'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  SessionSummary
newSessionSummary
  pAssistantArn_
  pAssistantId_
  pSessionArn_
  pSessionId_ =
    SessionSummary'
      { assistantArn = pAssistantArn_,
        assistantId = pAssistantId_,
        sessionArn = pSessionArn_,
        sessionId = pSessionId_
      }

-- | The Amazon Resource Name (ARN) of the Wisdom assistant.
sessionSummary_assistantArn :: Lens.Lens' SessionSummary Prelude.Text
sessionSummary_assistantArn = Lens.lens (\SessionSummary' {assistantArn} -> assistantArn) (\s@SessionSummary' {} a -> s {assistantArn = a} :: SessionSummary)

-- | The identifier of the Wisdom assistant.
sessionSummary_assistantId :: Lens.Lens' SessionSummary Prelude.Text
sessionSummary_assistantId = Lens.lens (\SessionSummary' {assistantId} -> assistantId) (\s@SessionSummary' {} a -> s {assistantId = a} :: SessionSummary)

-- | The Amazon Resource Name (ARN) of the session.
sessionSummary_sessionArn :: Lens.Lens' SessionSummary Prelude.Text
sessionSummary_sessionArn = Lens.lens (\SessionSummary' {sessionArn} -> sessionArn) (\s@SessionSummary' {} a -> s {sessionArn = a} :: SessionSummary)

-- | The identifier of the session.
sessionSummary_sessionId :: Lens.Lens' SessionSummary Prelude.Text
sessionSummary_sessionId = Lens.lens (\SessionSummary' {sessionId} -> sessionId) (\s@SessionSummary' {} a -> s {sessionId = a} :: SessionSummary)

instance Data.FromJSON SessionSummary where
  parseJSON =
    Data.withObject
      "SessionSummary"
      ( \x ->
          SessionSummary'
            Prelude.<$> (x Data..: "assistantArn")
            Prelude.<*> (x Data..: "assistantId")
            Prelude.<*> (x Data..: "sessionArn")
            Prelude.<*> (x Data..: "sessionId")
      )

instance Prelude.Hashable SessionSummary where
  hashWithSalt _salt SessionSummary' {..} =
    _salt `Prelude.hashWithSalt` assistantArn
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` sessionArn
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData SessionSummary where
  rnf SessionSummary' {..} =
    Prelude.rnf assistantArn
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf sessionArn
      `Prelude.seq` Prelude.rnf sessionId
