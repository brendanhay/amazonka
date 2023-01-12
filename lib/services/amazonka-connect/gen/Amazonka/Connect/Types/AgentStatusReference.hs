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
-- Module      : Amazonka.Connect.Types.AgentStatusReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AgentStatusReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the agent\'s status.
--
-- /See:/ 'newAgentStatusReference' smart constructor.
data AgentStatusReference = AgentStatusReference'
  { -- | The Amazon Resource Name (ARN) of the agent\'s status.
    statusArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the agent status.
    statusName :: Prelude.Maybe Prelude.Text,
    -- | The start timestamp of the agent\'s status.
    statusStartTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentStatusReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusArn', 'agentStatusReference_statusArn' - The Amazon Resource Name (ARN) of the agent\'s status.
--
-- 'statusName', 'agentStatusReference_statusName' - The name of the agent status.
--
-- 'statusStartTimestamp', 'agentStatusReference_statusStartTimestamp' - The start timestamp of the agent\'s status.
newAgentStatusReference ::
  AgentStatusReference
newAgentStatusReference =
  AgentStatusReference'
    { statusArn = Prelude.Nothing,
      statusName = Prelude.Nothing,
      statusStartTimestamp = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the agent\'s status.
agentStatusReference_statusArn :: Lens.Lens' AgentStatusReference (Prelude.Maybe Prelude.Text)
agentStatusReference_statusArn = Lens.lens (\AgentStatusReference' {statusArn} -> statusArn) (\s@AgentStatusReference' {} a -> s {statusArn = a} :: AgentStatusReference)

-- | The name of the agent status.
agentStatusReference_statusName :: Lens.Lens' AgentStatusReference (Prelude.Maybe Prelude.Text)
agentStatusReference_statusName = Lens.lens (\AgentStatusReference' {statusName} -> statusName) (\s@AgentStatusReference' {} a -> s {statusName = a} :: AgentStatusReference)

-- | The start timestamp of the agent\'s status.
agentStatusReference_statusStartTimestamp :: Lens.Lens' AgentStatusReference (Prelude.Maybe Prelude.UTCTime)
agentStatusReference_statusStartTimestamp = Lens.lens (\AgentStatusReference' {statusStartTimestamp} -> statusStartTimestamp) (\s@AgentStatusReference' {} a -> s {statusStartTimestamp = a} :: AgentStatusReference) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AgentStatusReference where
  parseJSON =
    Data.withObject
      "AgentStatusReference"
      ( \x ->
          AgentStatusReference'
            Prelude.<$> (x Data..:? "StatusArn")
            Prelude.<*> (x Data..:? "StatusName")
            Prelude.<*> (x Data..:? "StatusStartTimestamp")
      )

instance Prelude.Hashable AgentStatusReference where
  hashWithSalt _salt AgentStatusReference' {..} =
    _salt `Prelude.hashWithSalt` statusArn
      `Prelude.hashWithSalt` statusName
      `Prelude.hashWithSalt` statusStartTimestamp

instance Prelude.NFData AgentStatusReference where
  rnf AgentStatusReference' {..} =
    Prelude.rnf statusArn
      `Prelude.seq` Prelude.rnf statusName
      `Prelude.seq` Prelude.rnf statusStartTimestamp
