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
-- Module      : Network.AWS.AppFlow.Types.LookoutMetricsDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.LookoutMetricsDestinationProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties that are applied when Amazon Lookout for Metrics is used
-- as a destination.
--
-- /See:/ 'newLookoutMetricsDestinationProperties' smart constructor.
data LookoutMetricsDestinationProperties = LookoutMetricsDestinationProperties'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LookoutMetricsDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newLookoutMetricsDestinationProperties ::
  LookoutMetricsDestinationProperties
newLookoutMetricsDestinationProperties =
  LookoutMetricsDestinationProperties'

instance
  Core.FromJSON
    LookoutMetricsDestinationProperties
  where
  parseJSON =
    Core.withObject
      "LookoutMetricsDestinationProperties"
      ( \x ->
          Prelude.pure LookoutMetricsDestinationProperties'
      )

instance
  Prelude.Hashable
    LookoutMetricsDestinationProperties

instance
  Prelude.NFData
    LookoutMetricsDestinationProperties

instance
  Core.ToJSON
    LookoutMetricsDestinationProperties
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
