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
-- Module      : Amazonka.SWF.Types.ActivityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON ActivityType where
  parseJSON =
    Data.withObject
      "ActivityType"
      ( \x ->
          ActivityType'
            Prelude.<$> (x Data..: "name") Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable ActivityType where
  hashWithSalt _salt ActivityType' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData ActivityType where
  rnf ActivityType' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToJSON ActivityType where
  toJSON ActivityType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("version" Data..= version)
          ]
      )
