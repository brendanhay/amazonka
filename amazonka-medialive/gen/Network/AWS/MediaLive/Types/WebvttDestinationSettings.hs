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
-- Module      : Network.AWS.MediaLive.Types.WebvttDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.WebvttDestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Webvtt Destination Settings
--
-- /See:/ 'newWebvttDestinationSettings' smart constructor.
data WebvttDestinationSettings = WebvttDestinationSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WebvttDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newWebvttDestinationSettings ::
  WebvttDestinationSettings
newWebvttDestinationSettings =
  WebvttDestinationSettings'

instance Prelude.FromJSON WebvttDestinationSettings where
  parseJSON =
    Prelude.withObject
      "WebvttDestinationSettings"
      (\x -> Prelude.pure WebvttDestinationSettings')

instance Prelude.Hashable WebvttDestinationSettings

instance Prelude.NFData WebvttDestinationSettings

instance Prelude.ToJSON WebvttDestinationSettings where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
