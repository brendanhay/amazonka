{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.Scte27DestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte27DestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Scte27 Destination Settings
--
-- /See:/ 'newScte27DestinationSettings' smart constructor.
data Scte27DestinationSettings = Scte27DestinationSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte27DestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newScte27DestinationSettings ::
  Scte27DestinationSettings
newScte27DestinationSettings =
  Scte27DestinationSettings'

instance Prelude.FromJSON Scte27DestinationSettings where
  parseJSON =
    Prelude.withObject
      "Scte27DestinationSettings"
      (\x -> Prelude.pure Scte27DestinationSettings')

instance Prelude.Hashable Scte27DestinationSettings

instance Prelude.NFData Scte27DestinationSettings

instance Prelude.ToJSON Scte27DestinationSettings where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
