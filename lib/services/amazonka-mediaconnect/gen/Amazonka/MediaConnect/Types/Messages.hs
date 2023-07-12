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
-- Module      : Amazonka.MediaConnect.Types.Messages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Messages where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Messages that provide the state of the flow.
--
-- /See:/ 'newMessages' smart constructor.
data Messages = Messages'
  { -- | A list of errors that might have been generated from processes on this
    -- flow.
    errors :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Messages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'messages_errors' - A list of errors that might have been generated from processes on this
-- flow.
newMessages ::
  Messages
newMessages = Messages' {errors = Prelude.mempty}

-- | A list of errors that might have been generated from processes on this
-- flow.
messages_errors :: Lens.Lens' Messages [Prelude.Text]
messages_errors = Lens.lens (\Messages' {errors} -> errors) (\s@Messages' {} a -> s {errors = a} :: Messages) Prelude.. Lens.coerced

instance Data.FromJSON Messages where
  parseJSON =
    Data.withObject
      "Messages"
      ( \x ->
          Messages'
            Prelude.<$> (x Data..:? "errors" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Messages where
  hashWithSalt _salt Messages' {..} =
    _salt `Prelude.hashWithSalt` errors

instance Prelude.NFData Messages where
  rnf Messages' {..} = Prelude.rnf errors
