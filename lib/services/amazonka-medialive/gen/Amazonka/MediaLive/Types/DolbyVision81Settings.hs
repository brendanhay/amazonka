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
-- Module      : Amazonka.MediaLive.Types.DolbyVision81Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DolbyVision81Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dolby Vision81 Settings
--
-- /See:/ 'newDolbyVision81Settings' smart constructor.
data DolbyVision81Settings = DolbyVision81Settings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DolbyVision81Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDolbyVision81Settings ::
  DolbyVision81Settings
newDolbyVision81Settings = DolbyVision81Settings'

instance Data.FromJSON DolbyVision81Settings where
  parseJSON =
    Data.withObject
      "DolbyVision81Settings"
      (\x -> Prelude.pure DolbyVision81Settings')

instance Prelude.Hashable DolbyVision81Settings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DolbyVision81Settings where
  rnf _ = ()

instance Data.ToJSON DolbyVision81Settings where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
