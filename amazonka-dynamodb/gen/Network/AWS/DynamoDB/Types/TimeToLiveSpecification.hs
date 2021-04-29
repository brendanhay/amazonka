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
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the settings used to enable or disable Time to Live (TTL) for
-- the specified table.
--
-- /See:/ 'newTimeToLiveSpecification' smart constructor.
data TimeToLiveSpecification = TimeToLiveSpecification'
  { -- | Indicates whether TTL is to be enabled (true) or disabled (false) on the
    -- table.
    enabled :: Prelude.Bool,
    -- | The name of the TTL attribute used to store the expiration time for
    -- items in the table.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Bool ->
  -- | 'attributeName'
  Prelude.Text ->
  TimeToLiveSpecification
newTimeToLiveSpecification pEnabled_ pAttributeName_ =
  TimeToLiveSpecification'
    { enabled = pEnabled_,
      attributeName = pAttributeName_
    }

-- | Indicates whether TTL is to be enabled (true) or disabled (false) on the
-- table.
timeToLiveSpecification_enabled :: Lens.Lens' TimeToLiveSpecification Prelude.Bool
timeToLiveSpecification_enabled = Lens.lens (\TimeToLiveSpecification' {enabled} -> enabled) (\s@TimeToLiveSpecification' {} a -> s {enabled = a} :: TimeToLiveSpecification)

-- | The name of the TTL attribute used to store the expiration time for
-- items in the table.
timeToLiveSpecification_attributeName :: Lens.Lens' TimeToLiveSpecification Prelude.Text
timeToLiveSpecification_attributeName = Lens.lens (\TimeToLiveSpecification' {attributeName} -> attributeName) (\s@TimeToLiveSpecification' {} a -> s {attributeName = a} :: TimeToLiveSpecification)

instance Prelude.FromJSON TimeToLiveSpecification where
  parseJSON =
    Prelude.withObject
      "TimeToLiveSpecification"
      ( \x ->
          TimeToLiveSpecification'
            Prelude.<$> (x Prelude..: "Enabled")
            Prelude.<*> (x Prelude..: "AttributeName")
      )

instance Prelude.Hashable TimeToLiveSpecification

instance Prelude.NFData TimeToLiveSpecification

instance Prelude.ToJSON TimeToLiveSpecification where
  toJSON TimeToLiveSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Enabled" Prelude..= enabled),
            Prelude.Just
              ("AttributeName" Prelude..= attributeName)
          ]
      )
