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
-- Module      : Amazonka.KeySpaces.Types.TimeToLive
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.TimeToLive where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.TimeToLiveStatus
import qualified Amazonka.Prelude as Prelude

-- | Enable custom Time to Live (TTL) settings for rows and columns without
-- setting a TTL default for the specified table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_enabling Enabling TTL on tables>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- /See:/ 'newTimeToLive' smart constructor.
data TimeToLive = TimeToLive'
  { -- | Shows how to enable custom Time to Live (TTL) settings for the specified
    -- table.
    status :: TimeToLiveStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeToLive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'timeToLive_status' - Shows how to enable custom Time to Live (TTL) settings for the specified
-- table.
newTimeToLive ::
  -- | 'status'
  TimeToLiveStatus ->
  TimeToLive
newTimeToLive pStatus_ =
  TimeToLive' {status = pStatus_}

-- | Shows how to enable custom Time to Live (TTL) settings for the specified
-- table.
timeToLive_status :: Lens.Lens' TimeToLive TimeToLiveStatus
timeToLive_status = Lens.lens (\TimeToLive' {status} -> status) (\s@TimeToLive' {} a -> s {status = a} :: TimeToLive)

instance Data.FromJSON TimeToLive where
  parseJSON =
    Data.withObject
      "TimeToLive"
      (\x -> TimeToLive' Prelude.<$> (x Data..: "status"))

instance Prelude.Hashable TimeToLive where
  hashWithSalt _salt TimeToLive' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData TimeToLive where
  rnf TimeToLive' {..} = Prelude.rnf status

instance Data.ToJSON TimeToLive where
  toJSON TimeToLive' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )
