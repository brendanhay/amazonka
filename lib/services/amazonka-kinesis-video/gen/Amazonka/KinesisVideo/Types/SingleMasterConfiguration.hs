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
-- Module      : Amazonka.KinesisVideo.Types.SingleMasterConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.SingleMasterConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration for the @SINGLE_MASTER@
-- channel type.
--
-- /See:/ 'newSingleMasterConfiguration' smart constructor.
data SingleMasterConfiguration = SingleMasterConfiguration'
  { -- | The period of time a signaling channel retains undelivered messages
    -- before they are discarded.
    messageTtlSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleMasterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageTtlSeconds', 'singleMasterConfiguration_messageTtlSeconds' - The period of time a signaling channel retains undelivered messages
-- before they are discarded.
newSingleMasterConfiguration ::
  SingleMasterConfiguration
newSingleMasterConfiguration =
  SingleMasterConfiguration'
    { messageTtlSeconds =
        Prelude.Nothing
    }

-- | The period of time a signaling channel retains undelivered messages
-- before they are discarded.
singleMasterConfiguration_messageTtlSeconds :: Lens.Lens' SingleMasterConfiguration (Prelude.Maybe Prelude.Natural)
singleMasterConfiguration_messageTtlSeconds = Lens.lens (\SingleMasterConfiguration' {messageTtlSeconds} -> messageTtlSeconds) (\s@SingleMasterConfiguration' {} a -> s {messageTtlSeconds = a} :: SingleMasterConfiguration)

instance Data.FromJSON SingleMasterConfiguration where
  parseJSON =
    Data.withObject
      "SingleMasterConfiguration"
      ( \x ->
          SingleMasterConfiguration'
            Prelude.<$> (x Data..:? "MessageTtlSeconds")
      )

instance Prelude.Hashable SingleMasterConfiguration where
  hashWithSalt _salt SingleMasterConfiguration' {..} =
    _salt `Prelude.hashWithSalt` messageTtlSeconds

instance Prelude.NFData SingleMasterConfiguration where
  rnf SingleMasterConfiguration' {..} =
    Prelude.rnf messageTtlSeconds

instance Data.ToJSON SingleMasterConfiguration where
  toJSON SingleMasterConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MessageTtlSeconds" Data..=)
              Prelude.<$> messageTtlSeconds
          ]
      )
