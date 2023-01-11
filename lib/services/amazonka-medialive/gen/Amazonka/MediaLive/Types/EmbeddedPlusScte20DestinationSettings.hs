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
-- Module      : Amazonka.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.EmbeddedPlusScte20DestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Embedded Plus Scte20 Destination Settings
--
-- /See:/ 'newEmbeddedPlusScte20DestinationSettings' smart constructor.
data EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmbeddedPlusScte20DestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEmbeddedPlusScte20DestinationSettings ::
  EmbeddedPlusScte20DestinationSettings
newEmbeddedPlusScte20DestinationSettings =
  EmbeddedPlusScte20DestinationSettings'

instance
  Data.FromJSON
    EmbeddedPlusScte20DestinationSettings
  where
  parseJSON =
    Data.withObject
      "EmbeddedPlusScte20DestinationSettings"
      ( \x ->
          Prelude.pure EmbeddedPlusScte20DestinationSettings'
      )

instance
  Prelude.Hashable
    EmbeddedPlusScte20DestinationSettings
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    EmbeddedPlusScte20DestinationSettings
  where
  rnf _ = ()

instance
  Data.ToJSON
    EmbeddedPlusScte20DestinationSettings
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
