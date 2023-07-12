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
-- Module      : Amazonka.ElasticBeanstalk.Types.LaunchTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.LaunchTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon EC2 launch template.
--
-- /See:/ 'newLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { -- | The ID of the launch template.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'launchTemplate_id' - The ID of the launch template.
newLaunchTemplate ::
  LaunchTemplate
newLaunchTemplate =
  LaunchTemplate' {id = Prelude.Nothing}

-- | The ID of the launch template.
launchTemplate_id :: Lens.Lens' LaunchTemplate (Prelude.Maybe Prelude.Text)
launchTemplate_id = Lens.lens (\LaunchTemplate' {id} -> id) (\s@LaunchTemplate' {} a -> s {id = a} :: LaunchTemplate)

instance Data.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate' Prelude.<$> (x Data..@? "Id")

instance Prelude.Hashable LaunchTemplate where
  hashWithSalt _salt LaunchTemplate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData LaunchTemplate where
  rnf LaunchTemplate' {..} = Prelude.rnf id
