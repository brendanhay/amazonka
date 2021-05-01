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
-- Module      : Network.AWS.MediaLive.Types.AribDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AribDestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Arib Destination Settings
--
-- /See:/ 'newAribDestinationSettings' smart constructor.
data AribDestinationSettings = AribDestinationSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AribDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAribDestinationSettings ::
  AribDestinationSettings
newAribDestinationSettings = AribDestinationSettings'

instance Prelude.FromJSON AribDestinationSettings where
  parseJSON =
    Prelude.withObject
      "AribDestinationSettings"
      (\x -> Prelude.pure AribDestinationSettings')

instance Prelude.Hashable AribDestinationSettings

instance Prelude.NFData AribDestinationSettings

instance Prelude.ToJSON AribDestinationSettings where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
