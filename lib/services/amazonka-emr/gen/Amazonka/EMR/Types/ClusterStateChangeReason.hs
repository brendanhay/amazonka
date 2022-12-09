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
-- Module      : Amazonka.EMR.Types.ClusterStateChangeReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.ClusterStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | The reason that the cluster changed to its current state.
--
-- /See:/ 'newClusterStateChangeReason' smart constructor.
data ClusterStateChangeReason = ClusterStateChangeReason'
  { -- | The programmatic code for the state change reason.
    code :: Prelude.Maybe ClusterStateChangeReasonCode,
    -- | The descriptive message for the state change reason.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'clusterStateChangeReason_code' - The programmatic code for the state change reason.
--
-- 'message', 'clusterStateChangeReason_message' - The descriptive message for the state change reason.
newClusterStateChangeReason ::
  ClusterStateChangeReason
newClusterStateChangeReason =
  ClusterStateChangeReason'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The programmatic code for the state change reason.
clusterStateChangeReason_code :: Lens.Lens' ClusterStateChangeReason (Prelude.Maybe ClusterStateChangeReasonCode)
clusterStateChangeReason_code = Lens.lens (\ClusterStateChangeReason' {code} -> code) (\s@ClusterStateChangeReason' {} a -> s {code = a} :: ClusterStateChangeReason)

-- | The descriptive message for the state change reason.
clusterStateChangeReason_message :: Lens.Lens' ClusterStateChangeReason (Prelude.Maybe Prelude.Text)
clusterStateChangeReason_message = Lens.lens (\ClusterStateChangeReason' {message} -> message) (\s@ClusterStateChangeReason' {} a -> s {message = a} :: ClusterStateChangeReason)

instance Data.FromJSON ClusterStateChangeReason where
  parseJSON =
    Data.withObject
      "ClusterStateChangeReason"
      ( \x ->
          ClusterStateChangeReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable ClusterStateChangeReason where
  hashWithSalt _salt ClusterStateChangeReason' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ClusterStateChangeReason where
  rnf ClusterStateChangeReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
