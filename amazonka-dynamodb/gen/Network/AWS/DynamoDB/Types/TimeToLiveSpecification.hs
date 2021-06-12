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
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the settings used to enable or disable Time to Live (TTL) for
-- the specified table.
--
-- /See:/ 'newTimeToLiveSpecification' smart constructor.
data TimeToLiveSpecification = TimeToLiveSpecification'
  { -- | Indicates whether TTL is to be enabled (true) or disabled (false) on the
    -- table.
    enabled :: Core.Bool,
    -- | The name of the TTL attribute used to store the expiration time for
    -- items in the table.
    attributeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeToLiveSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'timeToLiveSpecification_enabled' - Indicates whether TTL is to be enabled (true) or disabled (false) on the
-- table.
--
-- 'attributeName', 'timeToLiveSpecification_attributeName' - The name of the TTL attribute used to store the expiration time for
-- items in the table.
newTimeToLiveSpecification ::
  -- | 'enabled'
  Core.Bool ->
  -- | 'attributeName'
  Core.Text ->
  TimeToLiveSpecification
newTimeToLiveSpecification pEnabled_ pAttributeName_ =
  TimeToLiveSpecification'
    { enabled = pEnabled_,
      attributeName = pAttributeName_
    }

-- | Indicates whether TTL is to be enabled (true) or disabled (false) on the
-- table.
timeToLiveSpecification_enabled :: Lens.Lens' TimeToLiveSpecification Core.Bool
timeToLiveSpecification_enabled = Lens.lens (\TimeToLiveSpecification' {enabled} -> enabled) (\s@TimeToLiveSpecification' {} a -> s {enabled = a} :: TimeToLiveSpecification)

-- | The name of the TTL attribute used to store the expiration time for
-- items in the table.
timeToLiveSpecification_attributeName :: Lens.Lens' TimeToLiveSpecification Core.Text
timeToLiveSpecification_attributeName = Lens.lens (\TimeToLiveSpecification' {attributeName} -> attributeName) (\s@TimeToLiveSpecification' {} a -> s {attributeName = a} :: TimeToLiveSpecification)

instance Core.FromJSON TimeToLiveSpecification where
  parseJSON =
    Core.withObject
      "TimeToLiveSpecification"
      ( \x ->
          TimeToLiveSpecification'
            Core.<$> (x Core..: "Enabled")
            Core.<*> (x Core..: "AttributeName")
      )

instance Core.Hashable TimeToLiveSpecification

instance Core.NFData TimeToLiveSpecification

instance Core.ToJSON TimeToLiveSpecification where
  toJSON TimeToLiveSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Enabled" Core..= enabled),
            Core.Just ("AttributeName" Core..= attributeName)
          ]
      )
