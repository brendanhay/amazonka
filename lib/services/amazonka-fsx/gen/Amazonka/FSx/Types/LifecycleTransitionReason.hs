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
-- Module      : Amazonka.FSx.Types.LifecycleTransitionReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LifecycleTransitionReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes why a resource lifecycle state changed.
--
-- /See:/ 'newLifecycleTransitionReason' smart constructor.
data LifecycleTransitionReason = LifecycleTransitionReason'
  { message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleTransitionReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'lifecycleTransitionReason_message' - Undocumented member.
newLifecycleTransitionReason ::
  LifecycleTransitionReason
newLifecycleTransitionReason =
  LifecycleTransitionReason'
    { message =
        Prelude.Nothing
    }

-- | Undocumented member.
lifecycleTransitionReason_message :: Lens.Lens' LifecycleTransitionReason (Prelude.Maybe Prelude.Text)
lifecycleTransitionReason_message = Lens.lens (\LifecycleTransitionReason' {message} -> message) (\s@LifecycleTransitionReason' {} a -> s {message = a} :: LifecycleTransitionReason)

instance Core.FromJSON LifecycleTransitionReason where
  parseJSON =
    Core.withObject
      "LifecycleTransitionReason"
      ( \x ->
          LifecycleTransitionReason'
            Prelude.<$> (x Core..:? "Message")
      )

instance Prelude.Hashable LifecycleTransitionReason where
  hashWithSalt _salt LifecycleTransitionReason' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData LifecycleTransitionReason where
  rnf LifecycleTransitionReason' {..} =
    Prelude.rnf message
