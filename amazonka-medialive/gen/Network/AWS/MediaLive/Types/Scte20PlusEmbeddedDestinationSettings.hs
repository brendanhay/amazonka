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
-- Module      : Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Scte20 Plus Embedded Destination Settings
--
-- /See:/ 'newScte20PlusEmbeddedDestinationSettings' smart constructor.
data Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte20PlusEmbeddedDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newScte20PlusEmbeddedDestinationSettings ::
  Scte20PlusEmbeddedDestinationSettings
newScte20PlusEmbeddedDestinationSettings =
  Scte20PlusEmbeddedDestinationSettings'

instance
  Prelude.FromJSON
    Scte20PlusEmbeddedDestinationSettings
  where
  parseJSON =
    Prelude.withObject
      "Scte20PlusEmbeddedDestinationSettings"
      ( \x ->
          Prelude.pure Scte20PlusEmbeddedDestinationSettings'
      )

instance
  Prelude.Hashable
    Scte20PlusEmbeddedDestinationSettings

instance
  Prelude.NFData
    Scte20PlusEmbeddedDestinationSettings

instance
  Prelude.ToJSON
    Scte20PlusEmbeddedDestinationSettings
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
