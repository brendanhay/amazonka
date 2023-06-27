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
-- Module      : Amazonka.LexV2Models.Types.ParentBotNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ParentBotNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A network of bots.
--
-- /See:/ 'newParentBotNetwork' smart constructor.
data ParentBotNetwork = ParentBotNetwork'
  { -- | The identifier of the network of bots assigned by Amazon Lex.
    botId :: Prelude.Text,
    -- | The version of the network of bots.
    botVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParentBotNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'parentBotNetwork_botId' - The identifier of the network of bots assigned by Amazon Lex.
--
-- 'botVersion', 'parentBotNetwork_botVersion' - The version of the network of bots.
newParentBotNetwork ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  ParentBotNetwork
newParentBotNetwork pBotId_ pBotVersion_ =
  ParentBotNetwork'
    { botId = pBotId_,
      botVersion = pBotVersion_
    }

-- | The identifier of the network of bots assigned by Amazon Lex.
parentBotNetwork_botId :: Lens.Lens' ParentBotNetwork Prelude.Text
parentBotNetwork_botId = Lens.lens (\ParentBotNetwork' {botId} -> botId) (\s@ParentBotNetwork' {} a -> s {botId = a} :: ParentBotNetwork)

-- | The version of the network of bots.
parentBotNetwork_botVersion :: Lens.Lens' ParentBotNetwork Prelude.Text
parentBotNetwork_botVersion = Lens.lens (\ParentBotNetwork' {botVersion} -> botVersion) (\s@ParentBotNetwork' {} a -> s {botVersion = a} :: ParentBotNetwork)

instance Data.FromJSON ParentBotNetwork where
  parseJSON =
    Data.withObject
      "ParentBotNetwork"
      ( \x ->
          ParentBotNetwork'
            Prelude.<$> (x Data..: "botId")
            Prelude.<*> (x Data..: "botVersion")
      )

instance Prelude.Hashable ParentBotNetwork where
  hashWithSalt _salt ParentBotNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion

instance Prelude.NFData ParentBotNetwork where
  rnf ParentBotNetwork' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
