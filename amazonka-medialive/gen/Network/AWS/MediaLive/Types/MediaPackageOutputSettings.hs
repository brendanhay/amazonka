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
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Media Package Output Settings
--
-- /See:/ 'newMediaPackageOutputSettings' smart constructor.
data MediaPackageOutputSettings = MediaPackageOutputSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaPackageOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newMediaPackageOutputSettings ::
  MediaPackageOutputSettings
newMediaPackageOutputSettings =
  MediaPackageOutputSettings'

instance Core.FromJSON MediaPackageOutputSettings where
  parseJSON =
    Core.withObject
      "MediaPackageOutputSettings"
      (\x -> Prelude.pure MediaPackageOutputSettings')

instance Prelude.Hashable MediaPackageOutputSettings

instance Prelude.NFData MediaPackageOutputSettings

instance Core.ToJSON MediaPackageOutputSettings where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
