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
-- Module      : Network.AWS.SWF.Types.ActivityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an activity type.
--
-- /See:/ 'newActivityType' smart constructor.
data ActivityType = ActivityType'
  { -- | The name of this activity.
    --
    -- The combination of activity type name and version must be unique within
    -- a domain.
    name :: Prelude.Text,
    -- | The version of this activity.
    --
    -- The combination of activity type name and version must be unique with in
    -- a domain.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'activityType_name' - The name of this activity.
--
-- The combination of activity type name and version must be unique within
-- a domain.
--
-- 'version', 'activityType_version' - The version of this activity.
--
-- The combination of activity type name and version must be unique with in
-- a domain.
newActivityType ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  ActivityType
newActivityType pName_ pVersion_ =
  ActivityType' {name = pName_, version = pVersion_}

-- | The name of this activity.
--
-- The combination of activity type name and version must be unique within
-- a domain.
activityType_name :: Lens.Lens' ActivityType Prelude.Text
activityType_name = Lens.lens (\ActivityType' {name} -> name) (\s@ActivityType' {} a -> s {name = a} :: ActivityType)

-- | The version of this activity.
--
-- The combination of activity type name and version must be unique with in
-- a domain.
activityType_version :: Lens.Lens' ActivityType Prelude.Text
activityType_version = Lens.lens (\ActivityType' {version} -> version) (\s@ActivityType' {} a -> s {version = a} :: ActivityType)

instance Prelude.FromJSON ActivityType where
  parseJSON =
    Prelude.withObject
      "ActivityType"
      ( \x ->
          ActivityType'
            Prelude.<$> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "version")
      )

instance Prelude.Hashable ActivityType

instance Prelude.NFData ActivityType

instance Prelude.ToJSON ActivityType where
  toJSON ActivityType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("version" Prelude..= version)
          ]
      )
